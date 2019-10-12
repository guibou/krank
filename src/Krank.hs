module Krank (
  processFile
  ) where

import qualified Krank.Checkers.IssueTracker as IT
import Krank.Types

processFile :: FilePath
            -> IO [Violation]
processFile filePath = do
  content <- readFile filePath
  IT.checkText content
