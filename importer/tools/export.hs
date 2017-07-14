-- Copyright (C) 2017 Red Hat, Inc.
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Conditional(cond, ifM, whenM, unlessM)
import           Control.Monad(unless, when)
import           Control.Monad.Except(ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource, runResourceT)
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit((.|), Conduit, Consumer, Producer, await, bracketP, runConduit, yield)
import           Data.Conduit.Binary(sinkFile, sinkLbs)
import qualified Data.Conduit.List as CL
import           Data.List(intercalate, isSuffixOf, isPrefixOf, partition)
import           Data.List.Split(splitOn)
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Database.Persist.Sql(SqlPersistT)
import           Database.Persist.Sqlite(runSqlite)
import           System.Directory(createDirectoryIfMissing, doesFileExist, doesPathExist, removeFile, removePathForcibly, renameFile, setModificationTime)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.IO.Temp(createTempDirectory)
import           System.Posix.Files(createSymbolicLink, setFileMode)
import           System.Posix.Types(CMode(..))
import           System.Process(callProcess)

import           GI.Gio(IsInputStream, inputStreamReadBytes, noCancellable)
import           GI.GLib(bytesGetData, bytesGetSize)
import           GI.OSTree(IsRepo)

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(groupIdToFilesC)
import           BDCS.Groups(nevraToGroupId)
import           BDCS.RPM.Utils(splitFilename)
import           BDCS.Version
import           Utils.Either(maybeToEither, whenLeft)
import           Utils.Monad(concatMapM)

import           Paths_db(getDataFileName)

-- Convert a GInputStream to a conduit source
sourceInputStream :: (MonadResource m, IsInputStream i) => i -> Producer m ByteString
sourceInputStream input = do
    let buf_size = 8096
    bytes <- liftIO $ inputStreamReadBytes input buf_size noCancellable
    bytesSize <- liftIO $ bytesGetSize bytes
    unless (bytesSize == 0) $ do
        bytesData <- liftIO $ bytesGetData bytes
        yield $ fromMaybe "" bytesData
        sourceInputStream input

getGroupIdC :: (MonadError String m, MonadBaseControl IO m, MonadIO m) => Conduit T.Text (SqlPersistT m) (Key Groups)
getGroupIdC = await >>= \case
    Nothing    -> return ()
    Just thing -> do
        maybeId <- lift $ nevraToGroupId (splitFilename thing)
        case maybeId of
            Just gid -> yield gid >> getGroupIdC
            Nothing  -> throwError $ "No such group " ++ T.unpack thing

filesToObjectsC :: (IsRepo a, MonadError String m, MonadIO m) => a -> Conduit Files m (Files, CS.Object)
filesToObjectsC repo = await >>= \case
    Nothing        -> return ()
    Just f@Files{..} -> case filesCs_object of
        Nothing       -> filesToObjectsC repo
        Just checksum -> do
            object <- CS.load repo checksum
            yield (f, object)
            filesToObjectsC repo

objectToTarEntry :: (MonadError String m, MonadIO m) => Conduit (Files, CS.Object) m Tar.Entry
objectToTarEntry = await >>= \case
    Nothing                 -> return ()
    Just (f@Files{..}, obj) -> do
        result <- case obj of
                CS.DirMeta dirmeta    -> return $ checkoutDir f dirmeta
                CS.FileObject fileObj -> liftIO . runExceptT $ checkoutFile f fileObj

        either (\e -> throwError $ "Could not checkout out object " ++ T.unpack filesPath ++ ": " ++ e)
               yield
               result

        objectToTarEntry
 where
    checkoutDir :: Files -> CS.Metadata -> Either String Tar.Entry
    checkoutDir f@Files{..} metadata@CS.Metadata{..} = do
        path <- Tar.toTarPath True (T.unpack filesPath)
        return $ setMetadata f metadata (Tar.directoryEntry path)

    checkoutSymlink :: Files -> CS.Metadata -> T.Text -> Either String Tar.Entry
    checkoutSymlink f@Files{..} metadata target = do
        path'   <- Tar.toTarPath False (T.unpack filesPath)
        target' <- maybeToEither ("Path is too long or contains invalid characters: " ++ T.unpack target)
                                 (Tar.toLinkTarget (T.unpack target))
        return $ setMetadata f metadata (Tar.simpleEntry path' (Tar.SymbolicLink target'))

    checkoutFile :: Files -> CS.FileContents -> ExceptT String IO Tar.Entry
    checkoutFile f CS.FileContents{symlink=Just target, ..} =
        ExceptT $ return $ checkoutSymlink f metadata target
    checkoutFile f@Files{..} CS.FileContents{symlink=Nothing, contents=Just c, ..} = do
        path         <- ExceptT $ return $ Tar.toTarPath False (T.unpack filesPath)
        lazyContents <- runResourceT $ runConduit $ sourceInputStream c .| sinkLbs

        return $ setMetadata f metadata (Tar.fileEntry path lazyContents)
    -- TODO?
    checkoutFile _ _ = throwError "Unhandled file type"

    setMetadata :: Files -> CS.Metadata -> Tar.Entry -> Tar.Entry
    setMetadata Files{..} metadata entry =
        entry { Tar.entryPermissions = CMode (CS.mode metadata),
                Tar.entryOwnership   = Tar.Ownership { Tar.ownerId = fromIntegral (CS.uid metadata),
                                                       Tar.groupId = fromIntegral (CS.gid metadata),
                                                       Tar.ownerName = "",
                                                       Tar.groupName = "" },
                Tar.entryTime = fromIntegral filesMtime }

tarSink :: MonadIO m => FilePath -> Consumer Tar.Entry m ()
tarSink out_path = do
    entries <- CL.consume
    liftIO $ BSL.writeFile out_path (Tar.write entries)

directorySink :: MonadIO m => FilePath -> Consumer (Files, CS.Object) m ()
directorySink outPath = await >>= \case
    Nothing                         -> return ()
    Just (f, CS.DirMeta dirmeta)    -> liftIO (checkoutDir f dirmeta)  >> directorySink outPath
    Just (f, CS.FileObject fileObj) -> liftIO (checkoutFile f fileObj) >> directorySink outPath
 where
    checkoutDir :: Files -> CS.Metadata -> IO ()
    checkoutDir f@Files{..} metadata = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        -- create the directory if it isn't there already
        createDirectoryIfMissing True fullPath

        setMetadata f fullPath metadata

    checkoutFile :: Files -> CS.FileContents -> IO ()
    checkoutFile f@Files{..} CS.FileContents{..} = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        createDirectoryIfMissing True $ takeDirectory fullPath

        -- Write the data or the symlink, depending
        -- Skip creating the symbolic link if the target already exists
        case (symlink, contents) of
            (Just symlinkTarget, _) -> unlessM (doesPathExist fullPath) (createSymbolicLink (T.unpack symlinkTarget) fullPath)
            (_, Just c)             -> do
                runResourceT $ runConduit $ sourceInputStream c .| sinkFile fullPath
                setMetadata f fullPath metadata
            -- TODO?
            _                       -> return ()

    setMetadata :: Files -> FilePath -> CS.Metadata -> IO ()
    setMetadata Files{..} fullPath CS.Metadata{..} = do
        -- set the mode
        setFileMode fullPath (CMode mode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs

qcow2Sink :: (MonadResource m, MonadIO m) => FilePath -> Consumer (Files, CS.Object) m ()
qcow2Sink outPath =
    -- Writing and importing a tar file probably will not work, because some rpms contain paths
    -- with symlinks (e.g., /lib64/libaudit.so.1 is expected to be written to /usr/lib64).
    -- Instead, export to a temp directory and convert that to qcow

    bracketP (createTempDirectory (takeDirectory outPath) "export")
        removePathForcibly
        (\tmpDir -> do
            -- Run the sink to create a directory export
            directorySink tmpDir

            -- Make the directory export something usable, hopefully
            liftIO $ runHacks tmpDir

            -- Run virt-make-fs to generate the qcow2
            liftIO $ callProcess "virt-make-fs" [tmpDir, outPath, "--format=qcow2", "--label=composer"]
        )

-- | Run filesystem hacks needed to make a directory tree bootable
runHacks :: FilePath -> IO ()
runHacks exportPath = do
    -- set a root password
    -- pre-crypted from "redhat"
    shadowRecs <- map (splitOn ":") <$> lines <$> readFile (exportPath </> "etc" </> "shadow")
    let newRecs = map (\rec -> if head rec == "root" then
                                ["root", "$6$3VLMX3dyCGRa.JX3$RpveyimtrKjqcbZNTanUkjauuTRwqAVzRK8GZFkEinbjzklo7Yj9Z6FqXNlyajpgCdsLf4FEQQKH6tTza35xs/"] ++ drop 2 rec
                               else
                                rec) shadowRecs
    writeFile (exportPath </> "etc" </> "shadow.new") (unlines $ map (intercalate ":") newRecs)
    renameFile (exportPath </> "etc" </> "shadow.new") (exportPath </> "etc" </> "shadow")

    -- create an empty machine-id
    writeFile (exportPath </> "etc" </> "machine-id") ""

    -- Install a sysusers.d config file, and run systemd-sysusers to implement it
    let sysusersDir = exportPath </> "usr" </> "lib" </> "sysusers.d"
    createDirectoryIfMissing True sysusersDir
    getDataFileName "sysusers-default.conf" >>= readFile >>= writeFile (sysusersDir </> "weldr.conf")
    callProcess "systemd-sysusers" ["--root", exportPath]

    -- Create a fstab stub
    writeFile (exportPath </> "etc" </> "fstab") "LABEL=composer / ext2 defaults 0 0"

-- | Check a list of strings to see if any of them are files
-- If it is, read it and insert its contents in its place
expandFileThings :: [String] -> IO [String]
expandFileThings = concatMapM isThingFile
  where
    isThingFile :: String ->  IO [String]
    isThingFile thing = ifM (doesFileExist thing)
                            (lines <$> readFile thing)
                            (return [thing])

usage :: IO ()
usage = do
    printVersion "export"
    putStrLn "Usage: export metadata.db repo dest thing [thing ...]"
    putStrLn "dest can be:"
    putStrLn "\t* A directory (which may or may not already exist)"
    putStrLn "\t* The name of a .tar file to be created"
    putStrLn "thing can be:"
    putStrLn "\t* The NEVRA of an RPM, e.g. filesystem-3.2-21.el7.x86_64"
    putStrLn "\t* A path to a file containing NEVRA of RPMs, 1 per line."
    -- TODO group id?
    exitFailure

needFilesystem :: IO ()
needFilesystem = do
    printVersion "export"
    putStrLn "ERROR: The tar needs to have the filesystem package included"
    exitFailure

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 4) usage

    let db_path = T.pack (argv !! 0)
    repo <- CS.open (argv !! 1)
    let out_path = argv !! 2
    allThings <- expandFileThings $ drop 3 argv

    let (match, otherThings) = partition (isPrefixOf "filesystem-") allThings
    when (length match < 1) needFilesystem
    let things = map T.pack $ match !! 0 : otherThings

    let (handler, objectSink) = cond [(".tar" `isSuffixOf` out_path,   (cleanupHandler out_path, objectToTarEntry .| tarSink out_path)),
                                      (".qcow2" `isSuffixOf` out_path, (cleanupHandler out_path, qcow2Sink out_path)),
                                      (otherwise,                      (print, directoryOutput out_path))]

    result <- runExceptT $ runSqlite db_path $ runConduit $ CL.sourceList things
        .| getGroupIdC
        .| groupIdToFilesC
        .| filesToObjectsC repo
        .| objectSink

    whenLeft result handler
 where
    directoryOutput :: MonadIO m => FilePath -> Consumer (Files, CS.Object) m ()
    directoryOutput path = do
        directorySink path
        liftIO $ runHacks path

    cleanupHandler :: Show a => FilePath -> a -> IO ()
    cleanupHandler path e = print e >>
        whenM (doesFileExist path) (removeFile path)
