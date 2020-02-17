module Krank.Types
  ( GithubKey (..),
    GitlabHost (..),
    GitlabKey (..),
    Violation (..),
    ViolationLevel (..),
    KrankConfig (..),
    SourcePos (..),
    Localized (..),
  )
where

import Data.Map (Map)
import Data.Text (Text)

newtype GithubKey = GithubKey Text deriving (Show)

newtype GitlabKey = GitlabKey Text deriving (Show)

newtype GitlabHost = GitlabHost Text deriving (Show, Ord, Eq)

data ViolationLevel = Info | Warning | Error deriving (Show)

data SourcePos
  = SourcePos
      { file :: FilePath,
        lineNumber :: Int,
        colNumber :: Int
      }
  deriving (Show, Eq, Ord)

-- | Represents a localized chunk of information
-- in a file
data Localized t
  = Localized
      { getLocation :: SourcePos,
        unLocalized :: t
      }
  deriving (Show, Eq)

data Violation
  = Violation
      { -- | A textual representation of the checker. Most of the time that's
        -- the chunck of text parsed
        checker :: Text,
        -- | The 'ViolationLevel' associated with the result
        level :: ViolationLevel,
        -- | A message describing the error
        message :: Text,
        -- | The position in the input sources of the chunck
        location :: SourcePos
      }
  deriving (Show)

data KrankConfig
  = KrankConfig
      { -- | The github oAuth token
        githubKey :: Maybe GithubKey,
        -- | The gitlab oAuth token
        gitlabKeys :: Map GitlabHost GitlabKey,
        -- | If 'True', all IO operations, such as HTTP requests, are ignored
        dryRun :: Bool,
        -- | Use color for formatting
        useColors :: Bool
      }
  deriving (Show)
