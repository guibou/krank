{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Krank.Checkers.IssueTracker (
  check
  , githubRE
  ) where

import Control.Applicative ((*>), optional)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import System.IO (readFile)
import Text.Regex.Applicative ((<|>), (=~), RE(), anySym, few, many, psym, some)

import Krank.Types

githubRE :: RE Char Int
githubRE = do
  optional ("http" *> optional "s" *> "://")
  optional "www."
  "github.com/"
  few (psym ('/' /=))
  "/"
  few (psym ('/' /=))
  "/"
  "issues/"
  issueNum <- some (psym isDigit)
  optional "/"
  " " <|> "\t" <|> "\n"
  return $ read issueNum

check :: FilePath
      -> IO [Int]
check file = do
  content <- readFile file
  print content
  let mMatches = content =~ many ((few anySym) *> githubRE <* (few anySym))
  pure $ fromMaybe [] mMatches
