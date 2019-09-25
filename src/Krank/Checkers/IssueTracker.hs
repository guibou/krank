{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Krank.Checkers.IssueTracker (
  GitIssue(..)
  , check
  , githubRE
  ) where

import Control.Applicative ((*>), optional)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import System.IO (readFile)
import Text.Regex.Applicative ((=~), RE(), anySym, few, many, psym, some)

import Krank.Types

data GitIssue = GitIssue {
  owner :: String,
  repo :: String,
  issueNum :: Int
} deriving (Eq, Show)

githubRE :: RE Char GitIssue
githubRE = do
  optional ("http" *> optional "s" *> "://")
  optional "www."
  "github.com/"
  repoOwner <- few (psym ('/' /=))
  "/"
  repoName <- few (psym ('/' /=))
  "/"
  "issues/"
  issueNumStr <- some (psym isDigit)
  -- Note that read is safe because of the regex parsing
  return $ GitIssue repoOwner repoName (read issueNumStr)

check :: FilePath
      -> IO [GitIssue]
check file = do
  content <- readFile file
  print content
  let mMatches = content =~ many ((few anySym) *> githubRE <* (few anySym))
  pure $ fromMaybe [] mMatches
