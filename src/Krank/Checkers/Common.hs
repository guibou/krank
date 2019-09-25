{-# LANGUAGE ApplicativeDo #-}

module Krank.Checkers.Common (
  multiple
  ) where

import Text.Regex.Applicative (RE(), anySym, few, many)

-- | Tries to match a RE multiply times, with anything in between the multiple possible matches
multiple :: RE a b
         -> RE a [b]
multiple re = many $ do
  few anySym
  match <- re
  few anySym
  pure match
