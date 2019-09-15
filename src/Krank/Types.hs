module Krank.Types (
  Violation
  ) where

import Data.Text (Text)

data Violation = Violation { checker :: Text
                           , level :: Int
                           , snippet :: Text
                           , message :: Text
                           } deriving (Show)
