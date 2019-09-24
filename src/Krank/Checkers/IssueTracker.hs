{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Krank.Checkers.IssueTracker (
  check
  , githubRE
  ) where

import Control.Applicative ((*>), optional)
import Data.Char (isDigit)
import Text.Regex.Applicative ((=~), RE(), anySym, few, psym)

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
  return $ read issueNum

check :: FilePath
      -> [Violation]
check file = []
