module Krank (
  processFile
  ) where

import qualified Krank.Checkers.IssueTracker as IT
import Krank.Types
import Control.Monad.Reader

processFile :: FilePath      -- ^ the file to analyze
            -> ReaderT KrankConfig IO [Violation]
processFile filePath = do
  content <- liftIO $ readFile filePath
  IT.checkText filePath content
