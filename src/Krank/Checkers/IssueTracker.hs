{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Krank.Checkers.IssueTracker (
  GitIssue(..)
  , check
  , extractIssues
  , githubRE
  , gitlabRE
  , gitRepoRE
  ) where

import Control.Applicative ((*>), optional)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import System.IO (readFile)
import Text.Regex.Applicative ((=~), RE(), anySym, few, many, psym, some, string)

import Krank.Types

data GitIssue = GitIssue {
  owner :: String,
  repo :: String,
  issueNum :: Int
} deriving (Eq, Show)

githubRE :: RE Char GitIssue
githubRE = gitRepoRE "github.com"

gitlabRE :: RE Char GitIssue
gitlabRE = gitRepoRE "gitlab.com"

gitRepoRE :: String
          -> RE Char GitIssue
gitRepoRE host = do
  optional ("http" *> optional "s" *> "://")
  optional "www."
  string host
  "/"
  repoOwner <- few (psym ('/' /=))
  "/"
  repoName <- few (psym ('/' /=))
  "/"
  "issues/"
  issueNumStr <- some (psym isDigit)
  -- Note that read is safe because of the regex parsing
  return $ GitIssue repoOwner repoName (read issueNumStr)

extractIssues :: String
              -> [GitIssue]
extractIssues toCheck =
  concat matches
    where
      patterns = [githubRE, gitlabRE]
      mMatches = (\x -> toCheck =~ many ((few anySym) *> x <* (few anySym))) <$> patterns
      matches = fromMaybe [] <$> mMatches

check :: FilePath
      -> IO [GitIssue]
check file = do
  content <- readFile file
  pure $ extractIssues content
