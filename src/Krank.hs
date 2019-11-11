{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.ByteString

import Krank.Formatter

processFile :: FilePath      -- ^ the file to analyze
            -> ReaderT KrankConfig IO [Violation]
processFile filePath = do
  content <- liftIO $ Data.ByteString.readFile filePath
  violations <- IT.checkText filePath content
  pure violations

runKrank :: [FilePath] -> KrankConfig -> IO ()
runKrank paths options = (flip runReaderT) options $ do
  KrankConfig{useColors} <- ask

  res <- (flip mapM) paths $ \path -> do
    (Right <$> processFile path) `catchAny`
      (\(SomeException e) -> pure $ Left [fmt|Error when processing {path}: {show e}|])


  liftIO $ (flip mapM_) res $ \case
    Left err -> Text.IO.hPutStrLn stderr err
    Right violations -> Text.IO.putStr (foldMap (showViolation useColors) violations)
