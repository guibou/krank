{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Krank.Formatter (
  showViolations
  ) where

import Data.Text (Text)
import PyF (fmt)
import Text.Megaparsec.Pos (sourcePosPretty)

import Krank.Types

showViolations :: [Violation]
               -> Text
showViolations = foldMap showViolation

showViolation :: Violation
              -> Text
showViolation violation = [fmt|
[{(show (level violation))}] {message violation}
    in: {snippet violation}
    file: {sourcePosPretty (location violation)}
|]
