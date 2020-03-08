{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Krank.Formatter
  ( showViolation,
  )
where

import Data.Text (Text)
import Krank.Types
import PyF (fmt)
import System.Console.Pretty
import Utils.Display (indent)

showViolation ::
  Bool ->
  Violation ->
  Text
showViolation useColors Violation {checker, location, level, message} =
  [fmt|
{showSourcePos location}: {showViolationLevel useColors level}:
{indent 2 checker}
{indent 4 message}
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

showSourcePos :: SourcePos -> String
showSourcePos (SourcePos path line column) = [fmt|{path}:{line}:{column}|]
