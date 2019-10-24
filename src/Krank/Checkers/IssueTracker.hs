{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Krank.Checkers.IssueTracker (
  GitIssue(..)
  , GitServer(..)
  , Localized(..)
  , checkText
  , extractIssues
  , githubRE
  -- , gitlabRE
  , gitRepoRE
  ) where

import Control.Applicative ((*>), optional)
import Control.Exception.Safe (catch)
import Data.Aeson (Value, (.:))
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (isDigit)
import Data.Text (Text, pack)
import qualified Network.HTTP.Req as Req
import PyF (fmt)

import Replace.Megaparsec
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import Data.Void
import Data.Either (rights)
import Control.Monad.Reader

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

localized :: Parser t -> Parser (Localized t)
localized p = Localized <$> getSourcePos <*> p

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
             -> String
serverDomain Github = "github.com"
-- serverDomain Gitlab = "gitlab.com"

type Parser t = Parsec Void String t

githubRE :: Parser GitIssue
githubRE = gitRepoRE Github

-- gitlabRE :: Parser GitIssue
-- gitlabRE = gitRepoRE Gitlab

gitRepoRE :: GitServer
          -> Parser GitIssue
gitRepoRE gitServer = do
  optional ("http" *> optional "s" *> "://")
  optional "www."
  string (serverDomain gitServer)
  "/"
  repoOwner <- some (satisfy ('/'/=))
  "/"
  repoName <- some (satisfy ('/'/=))
  "/"
  "issues/"
  issueNumStr <- some (satisfy isDigit)
  -- Note that read is safe because of the regex parsing
  return $ GitIssue gitServer (pack repoOwner) (pack repoName) (read issueNumStr)

extractIssues
  :: FilePath
  -> String
  -> [Localized GitIssue]
extractIssues filePath toCheck = case parse (findAllCap patterns) filePath toCheck of
  Left _ -> []
  Right res -> map snd $ rights res
  where
    patterns = localized $ choice [
      githubRE
      -- gitlabRE -- TODO: enable gitlab again
      ]

-- Supports only github for the moment
issueUrl :: GitIssue
         -> Req.Url 'Req.Https
issueUrl issue = case server issue of
  Github -> Req.https "api.github.com" Req./: "repos" Req./: owner issue Req./: repo issue Req./: "issues" Req./: (pack . show $ issueNum issue)
  -- Gitlab -> Req.https "google.com"

-- try Issue can fail, on non-2xx HTTP response
tryRestIssue :: Req.Url 'Req.Https
             -> ReaderT KrankConfig IO Value
tryRestIssue url = do
  mGithubKey <- githubKey <$> ask
  let
    authHeaders = case mGithubKey of
      Just (GithubKey token) -> Req.oAuth2Token (BSU.fromString token)
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
          -> String
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
