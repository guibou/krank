{-# LANGUAGE OverloadedStrings #-}

module Krank.Checkers.RunReq where

import qualified Network.HTTP.Req as Req
import Data.Aeson (Value)

-- | Runs a REST GET query on a specific host with custom headers and
--   return the JSON response.
--   This function should be as minimal as possible because it won't be tested.
--   Instead, a stub function replaces it for tests.
runRESTRequestReq :: Req.Url scheme -> Req.Option scheme -> IO Value
runRESTRequestReq url headers =
  Req.runReq Req.defaultHttpConfig $ do
    r <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse (
      Req.header "User-Agent" "krank"
      <> headers)
    pure $ Req.responseBody r
