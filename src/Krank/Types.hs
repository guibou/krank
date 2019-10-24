module Krank.Types (
  GithubKey(..)
  , Violation(..)
  , ViolationLevel(..)
  , KrankConfig(..)
  , SourcePos(..)
  ) where

import Data.Text (Text)

newtype GithubKey = GithubKey Text

data ViolationLevel = Info | Warning | Error deriving (Show)

data SourcePos = SourcePos FilePath Int Int
  deriving (Show, Eq)

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
  , dryRun :: Bool
  -- ^ If 'True', all IO operations, such as HTTP requests, are ignored
  , useColors :: Bool
  -- ^ Use color for formatting
  }
