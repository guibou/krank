{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Krank (
  runKrank
  ) where

import qualified Krank.Checkers.IssueTracker as IT
import Krank.Types
import Control.Monad.Reader
import Control.Exception.Safe
import PyF
import System.IO (hPutStrLn, stderr)
import Data.Text (unpack)

import Krank.Formatter

processFile :: FilePath      -- ^ the file to analyze
            -> ReaderT KrankConfig IO ()
processFile filePath = do
  KrankConfig{useColors} <- ask

  content <- liftIO $ readFile filePath
  violations <- IT.checkText filePath content
  liftIO $ putStr . unpack . foldMap (showViolation useColors) $ violations

runKrank :: [FilePath] -> KrankConfig -> IO ()
runKrank paths options = (flip runReaderT) options $ do
  (flip mapM_) paths $ \path -> do
    processFile path `catchAny`
      (\(SomeException e) -> liftIO $ hPutStrLn stderr [fmt|Error when processing {path}: {show e}|])
