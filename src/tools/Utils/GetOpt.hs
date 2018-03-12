{-# LANGUAGE FlexibleContexts #-}

module Utils.GetOpt(OptClass,
                    commandLineArgs,
                    compilerOpts)
 where

import Control.Monad.Except(MonadError, throwError)
import System.Console.GetOpt

class OptClass a

commandLineArgs :: [String] -> Maybe (String, FilePath, [String])
commandLineArgs (db:repo:args) = Just (db, repo, args)
commandLineArgs _              = Nothing

compilerOpts :: (MonadError String m, OptClass a) => [OptDescr (a -> a)] -> a -> [String] -> String -> m (a, [String])
compilerOpts options defaults argv cmdName =
    case getOpt Permute options argv of
        (o, n, [])   -> return (foldl (flip id) defaults o, n)
        (_, _, errs) -> throwError $ concat errs ++ usageInfo header options
 where
     header = "Usage: " ++ cmdName ++ " [OPTIONS]"
