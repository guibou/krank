{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Krank.Checkers.IssueTracker (
  GitIssue(..)
  , GitServer(..)
  , Localized(..)
  , checkText
  , extractIssues
  , findAll
  ) where

import Control.Exception.Safe (catch)
import Data.Aeson (Value, (.:))
import qualified Data.Aeson.Types as AesonT
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.HTTP.Req as Req
import PyF (fmt)

import Control.Monad.Reader
import Data.Char (isDigit)

import Krank.Types

data GitServer = Github
  -- Gitlab -- TODO: enable gitlab again
  deriving (Eq, Show)

data IssueStatus = Open | Closed deriving (Eq, Show)

-- | Represents a localized chunk of information
-- in a file
data Localized t = Localized
  { location :: SourcePos
  , unLocalized :: t
  } deriving (Show, Eq)

data GitIssue = GitIssue {
  server :: GitServer,
  owner :: Text,
  repo :: Text,
  issueNum :: Int
} deriving (Eq, Show)

data GitIssueWithStatus = GitIssueWithStatus {
  gitIssue :: Localized GitIssue,
  issueStatus :: IssueStatus
} deriving (Show)

serverDomain :: GitServer
             -> String
serverDomain Github = "github.com"
-- serverDomain Gitlab = "gitlab.com"

findAll :: Text -> [(Int, GitIssue)]
findAll l = go 1 (Text.splitOn "/" l)
  where
    go :: Int -> [Text] -> [(Int, GitIssue)]
    go col (scheme:"":domain:repoOwner:repoName:"issues":(Text.span isDigit->(numStr, rest)):xs)
    -- TODO: handle gitlab
    -- TODO: handle correct offset for "http:"
      | (("https:" `Text.isSuffixOf` scheme || "http:" `Text.isSuffixOf` scheme)) && not (Text.null numStr) && (domain == "github.com" || domain == "www.github.com") =
         (col + Text.length scheme - 6, GitIssue Github repoOwner repoName (read (Text.unpack numStr))) : go (col + Text.length scheme + Text.length domain + Text.length repoOwner + Text.length repoName + Text.length "issues" + Text.length numStr + 6) (rest:xs)
    go col (domain:repoOwner:repoName:"issues":(Text.span isDigit->(numStr, rest)):xs)
    -- TODO: handle gitlab
      | not (Text.null numStr) && (domain == "github.com" || domain == "www.github.com") =
         (col, GitIssue Github repoOwner repoName (read (Text.unpack numStr))) : go (col + Text.length domain + Text.length repoOwner + Text.length repoName + Text.length "issues" + Text.length numStr + 6) (rest:xs)
    go col (skipped:xs) = go (col + Text.length skipped + 1) xs
    go _ [] = []

extractIssues
  :: FilePath
  -> Text
  -> [Localized GitIssue]
extractIssues filePath toCheck = concat (zipWith f [1..] (Text.lines toCheck))
  where
    f lineNumber lineContent = map (\(col, x) -> Localized (SourcePos filePath lineNumber col) x) (findAll lineContent)

-- Supports only github for the moment
issueUrl :: GitIssue
         -> Req.Url 'Req.Https
issueUrl issue = case server issue of
  Github -> Req.https "api.github.com" Req./: "repos" Req./: owner issue Req./: repo issue Req./: "issues" Req./~ issueNum issue
  -- Gitlab -> Req.https "google.com"

-- try Issue can fail, on non-2xx HTTP response
tryRestIssue :: Req.Url 'Req.Https
             -> ReaderT KrankConfig IO Value
tryRestIssue url = do
  mGithubKey <- githubKey <$> ask
  let
    authHeaders = case mGithubKey of
      Just (GithubKey token) -> Req.oAuth2Token (Text.Encoding.encodeUtf8 token)
      Nothing -> mempty

  Req.runReq Req.defaultHttpConfig $ do
    r <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse (
      Req.header "User-Agent" "krank"
      <> authHeaders)
    pure $ Req.responseBody r


httpExcHandler :: Req.Url 'Req.Https
               -> Req.HttpException
               -> ReaderT KrankConfig IO Value
httpExcHandler url _ = pure . AesonT.object $ [("error", AesonT.String . pack . show $ url)]

restIssue :: Req.Url 'Req.Https
          -> ReaderT KrankConfig IO Value
restIssue url = catch (tryRestIssue url) (httpExcHandler url)

statusParser :: Value
            -> Either Text IssueStatus
statusParser (AesonT.Object o) = do
  let state :: AesonT.Result String = AesonT.parse (.: "state") o
  readState state
    where
      readState (AesonT.Success status) = case status of
        "closed" -> Right Closed
        "open"   -> Right Open
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
  let urls = issueUrl . unLocalized <$> issues
  statuses <- mapM restIssue urls
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
          -> Text
          -> ReaderT KrankConfig IO [Violation]
checkText path t = do
  let issues = extractIssues path t

  isDryRun <- dryRun <$> ask

  if isDryRun
    then pure $ fmap (\issue -> Violation {
                         checker = issuePrintUrl . unLocalized $ issue,
                         level = Info,
                         message = ("Dry run"),
                         location = location (issue :: Localized GitIssue)
                         }) issues
  else do
    issuesWithStatus <- gitIssuesWithStatus issues
    pure $ fmap f issuesWithStatus
      where
        f (Left (err, issue)) = Violation {
          checker = issuePrintUrl . unLocalized $ issue,
          level = Warning,
          message = ("Url could not be reached: " <> err),
          location = location (issue :: Localized GitIssue)
          }
        f (Right issue) = Violation {
          checker = issuePrintUrl (unLocalized . gitIssue $ issue),
          level = issueToLevel issue,
          message = issueToMessage issue,
          location = location ((gitIssue issue) :: Localized GitIssue)
          }
