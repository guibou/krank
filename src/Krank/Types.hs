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
                           , level :: ViolationLevel
                           , snippet :: Text
                           , message :: Text
                           , location :: SourcePos
                           } deriving (Show)

data KrankConfig = KrankConfig
  { githubKey :: Maybe GithubKey
  }
