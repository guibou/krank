{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Github
  ( showGithubException,
    githubAPILimitErrorText,
  )
where

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text, isPrefixOf)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Req as Req
import PyF (fmt)
import Utils.Req (showHTTPException, showRawResponse)

{- HLint ignore "Use newtype instead of data" -}

-- | Represents a typical Github Error serialized as JSON like so:
--
-- @
-- {
--    "message": "the error reason"
-- }
-- @
data GithubError
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
  case err of
    Just githubError -> showRawGithubMessage githubError
    Nothing -> showRawResponse resp body
  where
    err = decode $ fromStrict body :: Maybe GithubError

showRawGithubMessage ::
  GithubError ->
  Text
showRawGithubMessage err
  | isApiRateLimitError msg = githubAPILimitErrorText
  | otherwise = [fmt|From Github: {msg}|]
  where
    msg = message err

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
