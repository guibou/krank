{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Krank.Checkers.IssueTracker (
  GitIssue(..)
  , GitServer(..)
  , Localized(..)
  , checkText
  , extractIssues
  , gitRepoRe
  , serverDomain
  , extractIssuesOnALine
  ) where

import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Exception.Safe (catch)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (Value, (.:))
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.HTTP.Req as Req
import PyF (fmt)
import qualified Text.Regex.PCRE.Heavy as RE

import Krank.Types
import Utils.Req

data GitServer = Github | Gitlab GitlabHost
  deriving (Eq, Show)

data IssueStatus = Open | Closed deriving (Eq, Show)

data GitIssue = GitIssue {
  server :: GitServer,
  owner :: Text,
  repo :: Text,
  issueNum :: Int
} deriving (Eq, Show)

data GitIssueWithStatus = GitIssueWithStatus {
  gitIssue :: Localized GitIssue,
  issueStatus :: IssueStatus
} deriving (Eq, Show)

serverDomain :: GitServer
             -> Text
serverDomain Github = "github.com"
serverDomain (Gitlab (GitlabHost h)) = h

-- | This regex represents a github/gitlab issue URL
gitRepoRe :: RE.Regex
-- NOTE: \b at the beginning is really import for performances
-- because it dramatically reduces the number of backtracks
gitRepoRe = [RE.re|\b(?>https?://)?(?>www\.)?([^/ ]+)/([^/]+)/((?>[^/]+))/issues/([0-9]+)|]

-- | Extract all issues on one line and returns a list of the raw text associated with an issue
extractIssuesOnALine :: ByteString -> [(Int, GitIssue)]
extractIssuesOnALine lineContent = map f (RE.scan gitRepoRe lineContent)
      where
        f (match, [domain, owner, repo, ByteString.readInt -> Just (issueNo, _)]) = (colNo, GitIssue provider (Text.Encoding.decodeUtf8 owner) (Text.Encoding.decodeUtf8 repo) issueNo)
          where
            colNo = 1 + (ByteString.length $ fst $ ByteString.breakSubstring match lineContent)
            provider
              | domain == "github.com" = Github
              -- TODO: We suppose that all other cases are gitlab
              -- The only thing we risk here is a query with the wrong
              -- API to an irrelevant host.
              | otherwise = Gitlab (GitlabHost $ Text.Encoding.decodeUtf8 domain)

        -- This case seems impossible, the reasons for pattern match issues are:
        --  A number of items different than 4 in the list: there is only 4 matching groups in the regex
        --  An invalid `decimal` conversion. That's impossible either
        --  because the pattern for the issue number is `[0-9]+`
        f res = error ("Error: impossible match" <> show res)

-- | Extract all issues correctly localized
-- Note: we use 'ByteString' internally. This way we do not have to
-- care about the possible encoding of the input files.
-- In programming world, we mostly use ascii variants. This gives a
-- few performance improvement compared to initially converting
-- everything to 'Text' and search on it.
extractIssues
  :: FilePath
  -- ^ Path of the file
  -> ByteString
  -- ^ Content of the file
  -> [Localized GitIssue]
extractIssues filePath toCheck = concat (zipWith extract [1..] (ByteString.lines toCheck))
  where
    extract lineNo lineContent = map f (extractIssuesOnALine lineContent)
      where
        f (colNo, gitIssue) = Localized (SourcePos filePath lineNo colNo) gitIssue

-- Supports only github for the moment
issueUrl :: GitIssue
         -> Req.Url 'Req.Https
issueUrl issue = case server issue of
  Github -> Req.https "api.github.com" Req./: "repos" Req./: owner issue Req./: repo issue Req./: "issues" Req./~ issueNum issue
  Gitlab (GitlabHost host) -> Req.https host Req./: "api" Req./: "v4" Req./: "projects" Req./: [fmt|{owner issue}/{repo issue}|] Req./: "issues" Req./~ issueNum issue

-- try Issue can fail, on non-2xx HTTP response
tryRestIssue :: Localized GitIssue
             -> ReaderT KrankConfig IO Value
tryRestIssue locIssue = do
  let issue = unLocalized locIssue
  let url = issueUrl issue
  headers <- headersFor issue

  Req.runReq Req.defaultHttpConfig $ do
    r <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse (
      Req.header "User-Agent" "krank"
      <> headers)
    pure $ Req.responseBody r

headersFor :: GitIssue
           -> ReaderT KrankConfig IO (Req.Option 'Req.Https)
headersFor issue = do
  mGithubKey <- asks githubKey
  mGitlabKeys <- asks gitlabKeys

  case server issue of
    Github -> case mGithubKey of
      Just (GithubKey token) -> pure $ Req.oAuth2Token (Text.Encoding.encodeUtf8 token)
      Nothing -> pure mempty
    Gitlab host -> case Map.lookup host mGitlabKeys of
      Just (GitlabKey token) -> pure $ Req.header "PRIVATE-TOKEN" (Text.Encoding.encodeUtf8 token)
      Nothing -> pure mempty

httpExcHandler :: Localized GitIssue
               -> Req.HttpException
               -> ReaderT KrankConfig IO Value
httpExcHandler issue exc = pure . AesonT.object $ [("error", AesonT.String . pack $
  [fmt|
    Error:
      {(showHTTPException exc)}
  |])]

restIssue :: Localized GitIssue
          -> ReaderT KrankConfig IO Value
restIssue issue = catch (tryRestIssue issue) (httpExcHandler issue)

statusParser :: Value
            -> Either Text IssueStatus
statusParser (AesonT.Object o) = do
  let state :: AesonT.Result String = AesonT.parse (.: "state") o
  readState state
    where
      readState (AesonT.Success status) = case status of
        "closed" -> Right Closed        -- Both Gitlab and Github use the same keyword for closed
        "open"   -> Right Open          -- Github uses the 'open' status
        "opened" -> Right Open          -- Gitlab uses the 'opened' status
        _        -> Left [fmt|Could not parse status '{status}'|]
      readState (AesonT.Error _) = Left $ errorParser o
statusParser _ = Left "invalid JSON"

errorParser :: AesonT.Object
            -> Text
errorParser o = do
  let err = AesonT.parse (.: "error") o
  readErr err
    where
      readErr (AesonT.Success errText) = pack errText
      readErr (AesonT.Error _) = "invalid JSON"

gitIssuesWithStatus :: [Localized GitIssue]
                    -> ReaderT KrankConfig IO [Either (Text, Localized GitIssue) GitIssueWithStatus]
gitIssuesWithStatus issues = do
  statuses <- mapConcurrently restIssue issues
  pure $ zipWith f issues (fmap statusParser statuses)
    where
      f issue (Left err) = Left (err, issue)
      f issue (Right is) = Right $ GitIssueWithStatus issue is

issueToLevel :: GitIssueWithStatus
             -> ViolationLevel
issueToLevel i = case issueStatus i of
  Open   -> Info
  Closed -> Error

issueToMessage :: GitIssueWithStatus
               -> Text
issueToMessage i = case issueStatus i of
  Open   -> [fmt|still Open|]
  Closed -> [fmt|now Closed|]

issuePrintUrl :: GitIssue -> Text
issuePrintUrl GitIssue{owner, repo, server, issueNum} = [fmt|https://{serverDomain server}/{owner}/{repo}/issues/{issueNum}|]

checkText :: FilePath
          -> ByteString
          -> ReaderT KrankConfig IO [Violation]
checkText path t = do
  let issues = extractIssues path t

  isDryRun <- asks dryRun
  if isDryRun
    then pure $ fmap (\issue -> Violation {
                         checker = issuePrintUrl . unLocalized $ issue,
                         level = Info,
                         message = ("Dry run"),
                         location = getLocation (issue :: Localized GitIssue)
                         }) issues
  else do
    issuesWithStatus <- gitIssuesWithStatus issues
    pure $ fmap f issuesWithStatus
      where
        f (Left (err, issue)) = Violation {
          checker = issuePrintUrl . unLocalized $ issue,
          level = Warning,
          message = ("Url could not be reached: " <> err),
          location = getLocation (issue :: Localized GitIssue)
          }
        f (Right issue) = Violation {
          checker = issuePrintUrl (unLocalized . gitIssue $ issue),
          level = issueToLevel issue,
          message = issueToMessage issue,
          location = getLocation ((gitIssue issue) :: Localized GitIssue)
          }
