module Krank.Types (
  GithubKey(..)
  , Violation(..)
  , ViolationLevel(..)
  , KrankConfig(..)
  ) where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

newtype GithubKey = GithubKey String

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
  , dryRun :: Bool
  }
