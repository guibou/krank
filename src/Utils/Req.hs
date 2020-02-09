{-# LANGUAGE DataKinds #-}

module Utils.Req (
  showHTTPException
  ) where

import Data.Text (Text, pack)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Req as Req

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
-- Catch all doing a simple and ugly show
showExceptionContent exc = pack . show $ exc
