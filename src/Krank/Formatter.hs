{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

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
showViolation Violation{checker, location, level, message} = [fmt|
{sourcePosPretty $ location}: {showViolationLevel level}:
  {message}: {checker}
|]

showViolationLevel :: ViolationLevel -> String
showViolationLevel = \case
  Info -> "info"
  Warning -> "warning"
  Error -> "error"
