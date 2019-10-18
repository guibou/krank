module Krank.Types (
  GithubKey(..)
  , Violation(..)
  , ViolationLevel(..)
  ) where

import Data.Text (Text)

newtype GithubKey = GithubKey (Maybe String)

data ViolationLevel = Info | Warning | Error deriving (Show)

data Violation = Violation { checker :: Text
                           , level :: ViolationLevel
                           , snippet :: Text
                           , message :: Text
                           } deriving (Show)
