{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Krank.Formatter (
  showViolation
  ) where

import Data.Text (Text)
import PyF (fmt)
import Text.Megaparsec.Pos (sourcePosPretty)
import System.Console.Pretty

import Krank.Types

showViolation
  :: Bool
  -> Violation
  -> Text
showViolation useColors Violation{checker, location, level, message} = [fmt|
{sourcePosPretty location}: {showViolationLevel useColors level}:
  {message}: {checker}
|]

showViolationLevel :: Bool -> ViolationLevel -> String
showViolationLevel enableColor = \case
  Info -> colorized Green "info"
  Warning -> colorized Magenta "warning"
  Error -> colorized Red "error"
  where
    colorized c
      | enableColor = style Bold . color c
      | otherwise = id
