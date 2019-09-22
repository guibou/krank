module Krank.Types (
  Violation(..)
  , ViolationLevel(..)
  ) where

import Data.Text (Text)

data ViolationLevel = Info | Warning | Error deriving (Show)

data Violation = Violation { checker :: Text
                           , level :: ViolationLevel
                           , snippet :: Text
                           , message :: Text
                           } deriving (Show)
