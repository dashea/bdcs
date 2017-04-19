-- Copyright (C) 2016-2017 Red Hat, Inc.
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

module Import.RPM(consume,
                  loadIntoMDDB,
                  loadFromFile,
                  loadFromURL)
 where

import           Control.Monad(void)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader(ReaderT, ask)
import           Control.Monad.State(execStateT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit((.|), Consumer, await, runConduitRes)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import qualified Data.Text as T
import           GI.OSTree(IsRepo)
import           Network.HTTP.Conduit(path)
import           Network.HTTP.Simple(Request)

import BDCS.Builds(associateBuildWithPackage, insertBuild)
import BDCS.CS(commit, commitContents, store, withTransaction)
import BDCS.DB
import BDCS.Exceptions(DBException(..), throwIfNothing)
import BDCS.Files(associateFilesWithBuild, associateFilesWithPackage, insertFiles)
import BDCS.Packages(insertPackageName)
import BDCS.Projects(insertProject)
import BDCS.Signatures(insertBuildSignatures)
import BDCS.Sources(insertSource)
import BDCS.RPM.Builds(mkBuild)
import BDCS.RPM.Files(mkFiles)
import BDCS.RPM.Groups(createGroup)
import BDCS.RPM.Projects(mkProject)
import BDCS.RPM.Signatures(mkRSASignature, mkSHASignature)
import BDCS.RPM.Sources(mkSource)
import Import.Conduit(getFromFile, getFromURL)
import Import.State(ImportState(..))
import RPM.Parse(parseRPMC)
import RPM.Tags
import RPM.Types

buildImported :: MonadIO m => [Tag] ->  SqlPersistT m Bool
buildImported sigs =
    case findStringTag "SHA1Header" sigs of
        Just sha -> do ndx <- select $ from $ \signatures -> do
                              where_ $ signatures ^. BuildSignaturesSignature_type ==. val "SHA1" &&.
                                       signatures ^. BuildSignaturesSignature_data ==. val (C8.pack sha)
                              return $ signatures ^. BuildSignaturesId
                       return $ not $ null ndx
        Nothing  -> return False

-- A conduit consumer that takes in RPM data and stores its payload into the content store and its header
-- information into the mddb.  The return value is whether or not an import occurred.  This is not the
-- same as success vs. failure, as the import will be skipped if the package already exists in the mddb.
consume :: (IsRepo a, MonadIO m) => a -> FilePath -> Consumer RPM m Bool
consume repo db = await >>= \case
    Just rpm@RPM{..} -> do
        -- Query the MDDB to see if the package has already been imported.  If so, quit now to
        -- prevent it from being added to the content store a second time.  Note that load also
        -- performs this check, but both of these functions are public and therefore both need
        -- to prevent duplicate imports.
        let sigHeaders = headerTags $ head rpmSignatures
        existsInMDDB <- liftIO $ runSqlite (T.pack db) $ buildImported sigHeaders

        if existsInMDDB then return False
        else liftIO $ do
            let name = maybe "Unknown RPM" T.pack (findStringTag "Name" (headerTags $ head rpmHeaders))

            checksum <- withTransaction repo $ \r -> do
                f <- store r rpmArchive
                commit r f (T.concat ["Import of ", name, " into the repo"]) Nothing

            checksums <- execStateT (commitContents repo checksum) []
            loadIntoMDDB db rpm checksums

    Nothing -> return False

-- Load the headers from a parsed RPM into the mddb.  The return value is whether or not an import
-- occurred.  This is not the same as success vs. failure, as the import will be skipped if the
-- package already exists in the mddb.
loadIntoMDDB :: FilePath -> RPM -> [(T.Text, T.Text)] -> IO Bool
loadIntoMDDB db RPM{..} checksums = runSqlite (T.pack db) $ do
    let sigHeaders = headerTags $ head rpmSignatures
    let tagHeaders = headerTags $ head rpmHeaders

    existsInMDDB <- buildImported sigHeaders

    if existsInMDDB then return False
    else do
        projectId <- insertProject $ mkProject tagHeaders
        sourceId  <- insertSource $ mkSource tagHeaders projectId
        buildId   <- insertBuild $ mkBuild tagHeaders sourceId
        void $ insertBuildSignatures [mkRSASignature sigHeaders buildId, mkSHASignature sigHeaders buildId]
        filesIds  <- mkFiles tagHeaders checksums >>= insertFiles
        pkgNameId <- insertPackageName $ T.pack $ findStringTag "Name" tagHeaders `throwIfNothing` MissingRPMTag "Name"

        void $ associateFilesWithBuild filesIds buildId
        void $ associateFilesWithPackage filesIds pkgNameId
        void $ associateBuildWithPackage buildId pkgNameId

        -- groups and reqs
        -- groupId <- createGroup filesIds tagHeaders
        void $ createGroup filesIds tagHeaders
        return True

loadFromFile :: FilePath -> ReaderT ImportState IO ()
loadFromFile path = do
    db <- stDB <$> ask
    repo <- stRepo <$> ask

    result <- runExceptT $ runConduitRes (pipeline repo db path)
    case result of
        Right True -> liftIO $ putStrLn $ "Imported " ++ path
        _          -> return ()
 where
    pipeline r d f = getFromFile f .| parseRPMC .| consume r d

loadFromURL :: Request -> ReaderT ImportState IO ()
loadFromURL request = do
    db <- stDB <$> ask
    repo <- stRepo <$> ask

    result <- runExceptT $ runConduitRes (pipeline repo db request)
    case result of
        Right True -> liftIO $ C8.putStrLn $ BS.concat ["Imported ", path request]
        _          -> return ()
 where
    pipeline r d q = getFromURL q .| parseRPMC .| consume r d
