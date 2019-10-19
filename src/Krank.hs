module Krank (
  processFile
  ) where

import qualified Krank.Checkers.IssueTracker as IT
import Krank.Types

processFile :: FilePath      -- ^ the file to analyze
            -> Maybe GithubKey   -- ^ github developer key to circumvent API rate limit
            -> IO [Violation]
processFile filePath mGithubKey = do
  content <- readFile filePath
  IT.checkText content mGithubKey
