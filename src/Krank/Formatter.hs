{-# LANGUAGE OverloadedStrings #-}

module Krank.Formatter (
  showViolations
  ) where

import Data.Text (Text, append, pack)

import Krank.Types

showViolations :: [Violation]
               -> Text
showViolations = foldMap showViolation

showViolation :: Violation
              -> Text
showViolation violation = "\n"
  `append` "[" `append` pack (show (level violation)) `append` "]" `append` " " `append` message violation `append` "\n"
  `append` "    in: " `append` snippet violation
