{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Krank (
  runKrank
  ) where

import qualified Krank.Checkers.IssueTracker as IT
import Krank.Types
import Control.Monad.Reader
import Control.Exception.Safe
import PyF
import System.IO (stderr)
import qualified Data.Text.IO as Text.IO

import Krank.Formatter

processFile :: FilePath      -- ^ the file to analyze
            -> ReaderT KrankConfig IO ()
processFile filePath = do
  KrankConfig{useColors} <- ask

  content <- liftIO $ Text.IO.readFile filePath
  violations <- IT.checkText filePath content
  liftIO $ Text.IO.putStr . foldMap (showViolation useColors) $ violations

runKrank :: [FilePath] -> KrankConfig -> IO ()
runKrank paths options = (flip runReaderT) options $ do
  (flip mapM_) paths $ \path -> do
    processFile path `catchAny`
      (\(SomeException e) -> liftIO $ Text.IO.hPutStrLn stderr [fmt|Error when processing {path}: {show e}|])
