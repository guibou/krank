{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Utils.Req
-- Description : Utilities to handle Network.Req HTTP exceptions
module Utils.Req
  ( showHTTPException,
    showRawResponse,
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text, pack)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (Status (..))
import PyF (fmt)

showHTTPException ::
  -- | A function that accepts a StatusCodeException details and returns the appropriate text
  (Client.Response () -> ByteString -> Text) ->
  Req.HttpException ->
  Text
showHTTPException businessExcHandler (Req.VanillaHttpException clientHttpException) = showClientHttpException businessExcHandler clientHttpException
showHTTPException _ (Req.JsonHttpException exc) = pack exc

showClientHttpException ::
  -- | A function that accepts a StatusCodeException details and returns the appropriate text
  (Client.Response () -> ByteString -> Text) ->
  Client.HttpException ->
  Text
showClientHttpException businessExcHandler (Client.HttpExceptionRequest _ excContent) = showExceptionContent businessExcHandler excContent
showClientHttpException _ (Client.InvalidUrlException _ reason) = pack reason

showExceptionContent ::
  -- | A function that accepts a StatusCodeException details and returns the appropriate text
  (Client.Response () -> ByteString -> Text) ->
  Client.HttpExceptionContent ->
  Text
showExceptionContent businessExcHandler (Client.StatusCodeException resp body) = businessExcHandler resp body
-- Catch all doing a simple and ugly show
showExceptionContent _ exc = pack . show $ exc

showRawResponse ::
  Client.Response () ->
  ByteString ->
  Text
showRawResponse resp body =
  [fmt|\
HTTP call failed: {show status} - {show statusMsg}
      {show body}\
|]
  where
    status = statusCode . Client.responseStatus $ resp
    statusMsg = statusMessage . Client.responseStatus $ resp
