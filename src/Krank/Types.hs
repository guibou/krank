module Krank.Types (
  GithubKey(..)
  , GitlabKey(..)
  , GitlabHost(..)
  , Violation(..)
  , ViolationLevel(..)
  , KrankConfig(..)
  , SourcePos(..)
  ) where

import Data.Text (Text)
import Data.Map (Map)

newtype GithubKey = GithubKey Text deriving (Show)
newtype GitlabKey = GitlabKey Text deriving (Show)
newtype GitlabHost = GitlabHost Text deriving (Show, Ord, Eq)

data ViolationLevel = Info | Warning | Error deriving (Show)
data SourcePos = SourcePos { file :: FilePath
                           , lineNumber :: Int
                           , colNumber :: Int
                           } deriving (Show, Eq, Ord)

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
  , gitlabKeys :: Map GitlabHost GitlabKey
  -- ^ The gitlab oAuth token
  , dryRun :: Bool
  -- ^ If 'True', all IO operations, such as HTTP requests, are ignored
  , useColors :: Bool
  -- ^ Use color for formatting
  }
  deriving (Show)
