{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Gitlab
  ( showGitlabException,
    gitlabAPILimitErrorText,
    gitlabNotFoundErrorText,
  )
where

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text, isPrefixOf)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (Status (..))
import PyF (fmt)
import Utils.Req (showHTTPException, showRawResponse)

-- | Represents a typical Gitlab Error serialized as JSON like so:
--
-- @
-- {
--    "message": "the error reason"
-- }
-- @
newtype GitlabError
  = GitlabError
      { message :: Text
      }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Uses the helper to show generic HTTP issues and provides a specific handler for Gitlab
-- "business" exceptions
showGitlabException ::
  Req.HttpException ->
  Text
showGitlabException = showHTTPException handleGitlabException

handleGitlabException ::
  Client.Response () ->
  ByteString ->
  Text
handleGitlabException resp body =
  case statusCode (Client.responseStatus resp) of
    400 -> tryShowNiceErr
    404 -> gitlabNotFoundErrorText
    403 -> tryShowNiceErr
    _ -> showRawResponse resp body
  where
    err = decode $ fromStrict body :: Maybe GitlabError
    tryShowNiceErr = case err of
      Just gitlabError -> showRawGitlabMessage gitlabError
      Nothing -> showRawResponse resp body

showRawGitlabMessage ::
  GitlabError ->
  Text
showRawGitlabMessage err
  | isApiRateLimitError msg = gitlabAPILimitErrorText
  | otherwise = [fmt|From Gitlab: {msg}|]
  where
    msg = message err

gitlabNotFoundErrorText :: Text
gitlabNotFoundErrorText =
  [fmt|\
Could not find the indicated url.
It's possible that you have mistyped the URL
If not, this URL likely points to a private repository and you need to be authenticated to query its issues.
You might want to provide a gitlab API key with the --issuetracker-gitlabhost option.
See https://github.com/guibou/krank/blob/master/docs/Checkers/IssueTracker.md#gitlab|]

gitlabAPILimitErrorText :: Text
gitlabAPILimitErrorText =
  [fmt|\
Gitlab API Rate limit exceeded.
You might want to provide a gitlab API key with the --issuetracker-gitlabhost option.
See https://github.com/guibou/krank/blob/master/docs/Checkers/IssueTracker.md#gitlab|]

apiRateLimitPrefix :: Text
apiRateLimitPrefix = "API rate limit exceeded"

isApiRateLimitError ::
  Text ->
  Bool
isApiRateLimitError = isPrefixOf apiRateLimitPrefix
