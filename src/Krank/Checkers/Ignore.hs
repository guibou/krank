{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Krank.Checkers.Ignore ( IgnoreCommand(..)
                             , filterViolations
                             ) where

import PyF (fmt)

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.HashMap.Strict as HashM
import qualified Data.List as DataL
import qualified Text.Regex.PCRE.Heavy as RE

import Krank.Types

data IgnoreCommand = IgnoreLine deriving (Show, Eq)

-- | This regex represents a krank ignore marker
ignoreRe :: RE.Regex
ignoreRe = [RE.re|krank:ignore-(line)|]
-- TODO: support more "ignore" (checker specific, all file, next line)

-- | Extract all issues on one line and returns a list of ignore keyword
extractIssuesOnALine :: ByteString -> [(Int, IgnoreCommand)]
extractIssuesOnALine lineContent = Prelude.map f (RE.scan ignoreRe lineContent)
      where
        f (match, [command]) = (colNo, ignoreCommand)
          where
            colNo = 1 + ByteString.length (fst $ ByteString.breakSubstring match lineContent)
            ignoreCommand
              | command == "line" = IgnoreLine
              | otherwise = error [fmt|Impossible case, update the guard with: {ByteString.unpack command}|]

        -- This case seems impossible, the reasons for pattern match issues are:
        --  A number of items different than 1 in the list: there is only 1 matching groups in the regex
        f res = error ("Error: impossible match" <> show res)

-- | Extract all ignore markers correctly localized
-- Note: we use 'ByteString' internally. This way we do not have to
-- care about the possible encoding of the input files.
-- In programming world, we mostly use ascii variants. This gives a
-- few performance improvement compared to initially converting
-- everything to 'Text' and search on it.
extractIgnores
  :: FilePath
  -- ^ Path of the file
  -> ByteString
  -- ^ Content of the file
  -> [Localized IgnoreCommand]
extractIgnores filePath toCheck = concat (zipWith extract [1..] (ByteString.lines toCheck))
  where
    extract lineNo lineContent = Prelude.map f (extractIssuesOnALine lineContent)
      where
        f (colNo, gitIssue) = Localized (SourcePos filePath lineNo colNo) gitIssue

-- | Takes a list of Violation, some ignore commands and remove all those that are ignored due to an
-- ignore marker
filterViolations :: [Violation]
                 -- ^ List of Violation to filter
                 ->  FilePath
                 -- ^ Path of the file
                 -> ByteString
                 -- ^ Content of the file
                 -> [Violation]
filterViolations violations filePath content =
  DataL.filter isNotIgnored violations
  where
    ignoreCommands = extractIgnores filePath content
    f hashMap ignoreCommand = HashM.insert (lineNumber . getLocation $ ignoreCommand) (unLocalized ignoreCommand) hashMap
    ignoreIndex = foldl f HashM.empty ignoreCommands
    isIgnored violation = HashM.lookup (lineNumber . location $ violation) ignoreIndex == Just IgnoreLine
    isNotIgnored = not . isIgnored
