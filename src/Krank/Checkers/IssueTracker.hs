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
  , githubParser
  , gitlabParser
  , gitRepoParser
  , localized
  , serverDomain
  ) where

import Control.Applicative ((*>), optional)
import Control.Exception.Safe (catch)
import Data.Aeson (Value, (.:))
import qualified Data.Aeson.Types as AesonT
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.HTTP.Req as Req
import PyF (fmt)

import Replace.Megaparsec
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void
import Data.Either (rights)
import Control.Monad.Reader

import Krank.Types

data GitServer = Github | Gitlab
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
serverDomain Gitlab = "gitlab.com"

type Parser t = Parsec Void String t

githubParser :: Parser GitIssue
githubParser = gitRepoParser Github

gitlabParser :: Parser GitIssue
gitlabParser = gitRepoParser Gitlab

gitRepoParser :: GitServer
              -> Parser GitIssue
gitRepoParser gitServer = do
  optional ("http" *> optional (single 's') *> "://")
  optional "www."
  string (serverDomain gitServer)
  single '/'
  repoOwner <- takeWhile1P Nothing ('/'/=)
  single '/'
  repoName <- takeWhile1P Nothing ('/'/=)
  "/issues/"
  issueNum <- decimal
  return $ GitIssue gitServer (pack repoOwner) (pack repoName) issueNum

extractIssues
  :: FilePath
  -> String
  -> [Localized GitIssue]
extractIssues filePath toCheck = case parse (findAllCap patterns) filePath toCheck of
  Left _ -> []
  Right res -> map snd $ rights res
  where
    patterns = localized $ choice [
      -- `try` is important here and should appears before all the
      -- cases of the choices but the last one.

      -- imagine that you are parsing "(http://)?(github.com|gitlab.com)"
      -- and your input is http://gitlab.com
      -- it will first try the `githubParser` and match "http://",
      -- then it will fail with "gitlab.com" not being equal to
      -- "github.com". However it has already consumed a bit of the
      -- input string hence megaparsec will fail.

      -- This is especially misleading in our context because
      -- `findAllCap` will then try the parser on the tails of the
      -- input: ttp://gitlab.com, tp://gitlab.com, p://gitlab.com up
      -- until it tries on gitlab.com/...

      -- This input is different. It will first be tested against the
      -- `githubParser` expression. It won't find an "http" prefix and
      -- will directly switch to the match of "gitub.com", which will
      -- fail after consuming the prefix "gi". BUT `string` comes with
      -- a builtin `try`, which will reset the parser to the beginning
      -- of the input and then try the `gitlabParser`, which will
      -- succeed.
      try githubParser
      , gitlabParser
      ]

-- Supports only github for the moment
issueUrl :: GitIssue
         -> Req.Url 'Req.Https
issueUrl issue = case server issue of
  Github -> Req.https "api.github.com" Req./: "repos" Req./: owner issue Req./: repo issue Req./: "issues" Req./~ issueNum issue
  Gitlab -> Req.https "gitlab.com" Req./: "api" Req./: "v4" Req./: "projects" Req./: [fmt|{owner issue}/{repo issue}|] Req./: "issues" Req./~ issueNum issue

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
  mGitlabKey <- asks gitlabKey
  case server issue of
    Github -> case mGithubKey of
      Just (GithubKey token) -> pure $ Req.oAuth2Token (Text.Encoding.encodeUtf8 token)
      Nothing -> pure mempty
    Gitlab -> case mGitlabKey of
      Just (GitlabKey token) -> pure $ Req.header "PRIVATE-TOKEN" (Text.Encoding.encodeUtf8 token)
      Nothing -> pure mempty

httpExcHandler :: Localized GitIssue
               -> Req.HttpException
               -> ReaderT KrankConfig IO Value
httpExcHandler issue _ = pure . AesonT.object $ [("error", AesonT.String . pack . show $ issueUrl . unLocalized $ issue)]

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
  statuses <- mapM restIssue issues
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

  isDryRun <- asks dryRun
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
