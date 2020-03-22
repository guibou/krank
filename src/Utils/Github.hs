{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Github
  ( showGithubException,
    githubAPILimitErrorText,
    githubNotFoundErrorText,
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

-- | Represents a typical Github Error serialized as JSON like so:
--
-- @
-- {
--    "message": "the error reason"
-- }
-- @
newtype GithubError
  = GithubError
      { message :: Text
      }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Uses the helper to show generic HTTP issues and provides a specific handler for Github
-- "business" exceptions
showGithubException ::
  Req.HttpException ->
  Text
showGithubException = showHTTPException handleGithubException

handleGithubException ::
  Client.Response () ->
  ByteString ->
  Text
handleGithubException resp body =
  case statusCode (Client.responseStatus resp) of
    400 -> tryShowNiceErr
    404 -> githubNotFoundErrorText
    403 -> tryShowNiceErr
    _ -> showRawResponse resp body
  where
    err = decode $ fromStrict body :: Maybe GithubError
    tryShowNiceErr = case err of
      Just githubError -> showRawGithubMessage githubError
      Nothing -> showRawResponse resp body

showRawGithubMessage ::
  GithubError ->
  Text
showRawGithubMessage err
  | isApiRateLimitError msg = githubAPILimitErrorText
  | otherwise = [fmt|From Github: {msg}|]
  where
    msg = message err

githubNotFoundErrorText :: Text
githubNotFoundErrorText =
  [fmt|\
Could not find the indicated url.
It's possible that you have mistyped the URL
If not, this URL likely points to a private repository and you need to be authenticated to query its issues.
You might want to provide a github API key with the --issuetracker-githubkey option.
See https://github.com/guibou/krank/blob/master/docs/Checkers/IssueTracker.md#private-repositories|]

githubAPILimitErrorText :: Text
githubAPILimitErrorText =
  [fmt|\
Github API Rate limit exceeded.
You might want to provide a github API key with the --issuetracker-githubkey option.
See https://github.com/guibou/krank/blob/master/docs/Checkers/IssueTracker.md#api-rate-limitation|]

apiRateLimitPrefix :: Text
apiRateLimitPrefix = "API rate limit exceeded"

isApiRateLimitError ::
  Text ->
  Bool
isApiRateLimitError = isPrefixOf apiRateLimitPrefix
