module Krank.Types (
  GithubKey(..)
  , GitlabKey(..)
  , Violation(..)
  , ViolationLevel(..)
  , KrankConfig(..)
  ) where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

newtype GithubKey = GithubKey Text
newtype GitlabKey = GitlabKey Text

data ViolationLevel = Info | Warning | Error deriving (Show)

data Violation = Violation { checker :: Text
                             -- ^ A textual representation of the checker. Most of the time that's
                             -- the chunck of text parsed
                           , level :: ViolationLevel
                           -- ^ The 'ViolationLevel' associated with the result
                           , message :: Text
                           -- ^ A message describing the error
                           , location :: SourcePos
                           -- ^ The position in the input sources of the chunck
                           } deriving (Show)

data KrankConfig = KrankConfig
  { githubKey :: Maybe GithubKey
  -- ^ The github oAuth token
  , gitlabKey :: Maybe GitlabKey
  -- ^ The gitlab oAuth token
  , dryRun :: Bool
  -- ^ If 'True', all IO operations, such as HTTP requests, are ignored
  , useColors :: Bool
  -- ^ Use color for formatting
  }
