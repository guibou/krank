{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Display
  ( indent,
  )
where

import qualified Data.Text as Text
import PyF (fmt)

-- | indent the text given by a certain number of space character
-- If the text given contains multiple lines, all the lines but the first will be prefixed by the
-- continuation character '|'
indent :: Int -> Text.Text -> Text.Text
indent nbSpaces text = indentedText
  where
    (firstLine : nextLines) = Text.splitOn "\n" text
    prefixedLines = map (\a -> [fmt|| {a}|]) nextLines
    indentedLines = map (\a -> [fmt|{replicate nbSpaces ' '}{a}|]) (firstLine : prefixedLines)
    indentedText = Text.intercalate "\n" indentedLines
