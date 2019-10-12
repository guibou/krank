{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Krank.Checkers.IssueTracker (
  GitIssue(..)
  , GitServer(..)
  , checkFile
  , checkText
  , extractIssues
  , githubRE
  , gitlabRE
  , gitRepoRE
  ) where

import Control.Applicative ((*>), optional)
import Data.Aeson (Value, (.:))
import qualified Data.Aeson.Types as AesonT
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Network.HTTP.Req as Req
import PyF (fmt)
import System.IO (readFile)
import Text.Regex.Applicative ((=~), RE(), few, psym, some, string)

import Krank.Checkers.Common
import Krank.Types

data GitServer = Github | Gitlab deriving (Eq, Show)

data IssueStatus = Open | Closed deriving (Eq, Show)

data GitIssue = GitIssue {
  server :: GitServer,
  owner :: Text,
  repo :: Text,
  issueNum :: Int
} deriving (Eq, Show)

data GitIssueWithStatus = GitIssueWithStatus {
  gitIssue :: GitIssue,
  issueStatus :: IssueStatus
} deriving (Eq, Show)

serverDomain :: GitServer
             -> String
serverDomain Github = "github.com"
serverDomain Gitlab = "gitlab.com"

githubRE :: RE Char GitIssue
githubRE = gitRepoRE Github

gitlabRE :: RE Char GitIssue
gitlabRE = gitRepoRE Gitlab

gitRepoRE :: GitServer
          -> RE Char GitIssue
gitRepoRE gitServer = do
  optional ("http" *> optional "s" *> "://")
  optional "www."
  string (serverDomain gitServer)
  "/"
  repoOwner <- few (psym ('/' /=))
  "/"
  repoName <- few (psym ('/' /=))
  "/"
  "issues/"
  issueNumStr <- some (psym isDigit)
  -- Note that read is safe because of the regex parsing
  return $ GitIssue gitServer (pack repoOwner) (pack repoName) (read issueNumStr)

extractIssues :: String
              -> [GitIssue]
extractIssues toCheck =
  concat matches
    where
      patterns = [githubRE, gitlabRE]
      mMatches = (=~) toCheck . multiple <$> patterns
      matches = fromMaybe [] <$> mMatches

-- Supports only github for the moment
issueUrl :: GitIssue
         -> Req.Url 'Req.Https
issueUrl issue = case server issue of
  Github -> Req.https "api.github.com" Req./: "repos" Req./: owner issue Req./: repo issue Req./: "issues" Req./: (pack . show $ issueNum issue)
  Gitlab -> Req.https "google.com"

restIssue :: Req.Url 'Req.Https
            -> IO Value
restIssue url = Req.runReq Req.defaultHttpConfig $ do
  r <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse (Req.header "User-Agent" "krank")
  pure $ Req.responseBody r

getStatus :: Value
          -> AesonT.Result String
getStatus (AesonT.Object o) = AesonT.parse (.: "state") o
getStatus _ = AesonT.Error "invalid JSON"

parseStatus :: AesonT.Result String
            -> Either Text IssueStatus
parseStatus (AesonT.Success status) = case status of
  "closed" -> Right Closed
  "open"   -> Right Open
  _        -> Left [fmt|Could not parse status '{status}'|]
parseStatus (AesonT.Error err) = Left (pack err)

gitIssuesWithStatus :: [GitIssue]
                    -> IO [Either Text GitIssueWithStatus]
gitIssuesWithStatus issues = do
  let urls = issueUrl <$> issues
  statuses <- mapM restIssue urls
  pure $ zipWith f issues (fmap (parseStatus . getStatus) statuses)
    where
      f _ (Left err) = Left err
      f issue (Right is) = Right $ GitIssueWithStatus issue is

issueTrackerChecker :: Text
issueTrackerChecker = "GIT Issue Tracker"

issueToLevel :: GitIssueWithStatus
             -> ViolationLevel
issueToLevel i = case issueStatus i of
  Open   -> Info
  Closed -> Error

issueToSnippet :: GitIssueWithStatus
               -> Text
issueToSnippet i = [fmt|{owner issue}/{repo issue}|]
  where
    issue = gitIssue i

issueToMessage :: GitIssueWithStatus
               -> Text
issueToMessage i = case issueStatus i of
  Open   -> [fmt|issue #{issueNum issue} still Open|]
  Closed -> [fmt|issue #{issueNum issue} is now Closed|]
  where
    issue = gitIssue i

checkText :: String
          -> IO [Violation]
checkText t = do
  let issues = extractIssues t
  issuesWithStatus <- gitIssuesWithStatus issues
  pure $ fmap f issuesWithStatus
    where
      f (Left err) = Violation issueTrackerChecker Warning "Check error" err
      f (Right issue) = Violation issueTrackerChecker (issueToLevel issue) (issueToSnippet issue) (issueToMessage issue)

checkFile :: FilePath
      -> IO [Violation]
checkFile file = do
  content <- readFile file
  checkText content
