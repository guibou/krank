{-# LANGUAGE QuasiQuotes #-}

module Version
  ( displayVersion,
  )
where

import Data.Version (showVersion)
import Paths_krank (version)
import PyF (fmt)

getVersion :: String
getVersion = showVersion version

displayVersion :: String
displayVersion = [fmt|Krank {getVersion}|]
