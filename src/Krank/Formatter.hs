{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Krank.Formatter (
  showViolations
  ) where

import Data.Text (Text)
import PyF (f)

import Krank.Types

showViolations :: [Violation]
               -> Text
showViolations = foldMap showViolation

showViolation :: Violation
              -> Text
showViolation violation = [f|
[{(show (level violation))}] {message violation}
    in: {snippet violation}
|]
