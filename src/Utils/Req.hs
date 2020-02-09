{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Req (
  showHTTPException
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text, pack)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (Status(..))

import PyF (fmt)

showHTTPException :: Req.HttpException
                  -> Text
showHTTPException (Req.VanillaHttpException clientHttpException) = showClientHttpException clientHttpException
showHTTPException (Req.JsonHttpException exc) = pack exc

showClientHttpException :: Client.HttpException
                        -> Text
showClientHttpException (Client.HttpExceptionRequest _ excContent) = showExceptionContent excContent
showClientHttpException (Client.InvalidUrlException _ reason) = pack reason

showExceptionContent :: Client.HttpExceptionContent
                     -> Text
showExceptionContent (Client.StatusCodeException resp body) = showResponse resp body
-- Catch all doing a simple and ugly show
showExceptionContent exc = pack . show $ exc

showResponse :: Client.Response ()
             -> ByteString
             -> Text
showResponse resp body = [fmt|\
HTTP call failed: {show status} - {show statusMsg}
      {show body}\
|]
  where status = statusCode . Client.responseStatus $ resp
        statusMsg = statusMessage . Client.responseStatus $ resp
