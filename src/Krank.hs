{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Krank (
  runKrank
  ) where

import Krank.Checkers.Ignore (filterViolations)
import qualified Krank.Checkers.IssueTracker as IT
import Krank.Types
import Control.Monad.Reader
import Control.Exception.Safe
import PyF
import System.IO (stderr)
import qualified Data.Text.IO as Text.IO
import qualified Data.ByteString

import Control.Concurrent.Async.Lifted
import Krank.Formatter

processFile :: FilePath      -- ^ the file to analyze
            -> ReaderT KrankConfig IO [Violation]
processFile filePath = do
  content <- liftIO $ Data.ByteString.readFile filePath
  violations <- IT.checkText filePath content
  let filtered = filterViolations violations filePath content

  -- forcing 'violations' to WHNF forces more of the processing to happen inside the thread and
  -- improves a bit the runtime performances in parallel.
  -- forcing to Normal Form (with deepseq) does not bring anymore improvement
  pure $! filtered

runKrank :: [FilePath] -> KrankConfig -> IO ()
runKrank paths options = (flip runReaderT) options $ do
  KrankConfig{useColors} <- ask

  res <- forConcurrently paths $ \path -> do
    (Right <$> processFile path) `catchAny`
      (\(SomeException e) -> pure $ Left [fmt|Error when processing {path}: {show e}|])

  liftIO $ (flip mapM_) res $ \case
    Left err -> Text.IO.hPutStrLn stderr err
    Right violations -> Text.IO.putStr (foldMap (showViolation useColors) violations)
