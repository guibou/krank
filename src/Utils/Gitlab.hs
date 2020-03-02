module Utils.Gitlab
  ( showGitlabException,
  )
where

import Data.Text (Text)
import qualified Network.HTTP.Req as Req
import Utils.Req (showHTTPException, showRawResponse)

showGitlabException ::
  Req.HttpException ->
  Text
showGitlabException = showHTTPException showRawResponse
