{-# LANGUAGE OverloadedStrings #-}

module Krank.Checkers.RunReq where

import qualified Network.HTTP.Req as Req
import Data.Aeson (Value)

runRESTRequest :: Req.Url scheme -> Req.Option scheme -> IO Value
runRESTRequest url headers =
  Req.runReq Req.defaultHttpConfig $ do
    r <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse (
      Req.header "User-Agent" "krank"
      <> headers)
    pure $ Req.responseBody r
