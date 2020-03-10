{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Krank.Checkers.IssueTrackerSpec
  ( spec,
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow, SomeException, throw)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson (Result (..), Value (..), fromJSON, object)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
import Krank
import Krank.Checkers.IssueTracker
import Krank.Types
import qualified Network.HTTP.Req as Req
import PyF (fmt)
import Test.Hspec

data TestEnv
  = TestEnv
      { envFiles :: Map FilePath Data.ByteString.ByteString,
        envRestAnswers :: Map (Req.Url 'Req.Https) (Either Req.HttpException Value)
      }

newtype TestKrank t
  = TestKrank
      { unTestKrank :: WriterT ([Text], [Text]) (ReaderT (TestEnv, KrankConfig) (Either SomeException)) t
      }
  deriving newtype (Monad, Applicative, Functor, MonadThrow, MonadCatch)

-- | "pure" instance of 'MonadKrank'
-- It works on a 'TestEnv'.
instance MonadKrank TestKrank where

  krankPutStrLnStderr t = TestKrank $ tell ([], [t])

  krankPutStr t = TestKrank $ tell ([t], [])

  krankAsks f = TestKrank $ asks (f . snd)

  krankMapConcurrently f l = TestKrank $ mapM (coerce . f) l

  krankReadFile path = TestKrank $ do
    files <- asks (envFiles . fst)
    case Map.lookup path files of
      Nothing -> throw (userError "file not found")
      Just c -> pure c

  -- Note: options are ignored
  krankRunRESTRequest url _options = TestKrank $ do
    restAnswers <- asks (envRestAnswers . fst)
    case Map.lookup url restAnswers of
      Nothing -> error ("Answer not specified for this query: " <> show url)
      Just a -> case a of
        Right json -> case fromJSON json of
          Success res -> pure res
          Data.Aeson.Error s -> throw (Req.JsonHttpException s)
        Left exception -> throw exception

check :: ByteString -> Maybe GitIssue
check a = case extractIssuesOnALine a of
  [(_, x)] -> Just x
  _ -> Nothing

giturlTests :: GitServer -> Spec
giturlTests domain = do
  let domainName = serverDomain domain
  it "handles full https url" $ do
    let match = check [fmt|https://{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssue domain "guibou" "krank" 1)
  it "handles full http url" $ do
    let match = check [fmt|http://{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssue domain "guibou" "krank" 1)
  it "handles short url - no protocol" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssue domain "guibou" "krank" 1)
  it "accepts www. in url" $ do
    let match = check [fmt|https://www.{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssue domain "guibou" "krank" 1)
  it "accepts www in url - no protocol" $ do
    let match = check [fmt|www.{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssue domain "guibou" "krank" 1)
  it "fails if the issue number is not an int" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/foo|]
    match `shouldBe` Nothing
  it "fails on partial match" $ do
    let match = check [fmt|{domainName}/guibou/krank/|]
    match `shouldBe` Nothing
  it "fails on partial match (just missing the issue number)" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/|]
    match `shouldBe` Nothing
  it "handles full https url" $ do
    let match = check [fmt|https://{domainName}/guibou/krank/issues/2|]
    match `shouldBe` Just (GitIssue domain "guibou" "krank" 2)

spec :: Spec
spec = do
  context "Test.Krank.Checkers.specIssueTracker" $ do
    describe "#githubParser" $
      giturlTests Github
    describe "#githlabParser" $
      giturlTests (Gitlab (GitlabHost "gitlab.com"))
    describe "#extractIssues"
      $ it "handles both github and gitlab"
      $ do
        let match =
              extractIssues
                "localFile"
                [fmt|https://github.com/guibou/krank/issues/2
        some text
        https://gitlab.com/gitlab-org/gitlab-foss/issues/67390
        and more github https://github.com/guibou/krank/issues/1
        lalala https://gitlab.haskell.org/ghc/ghc/issues/16955
        |]
        match
          `shouldMatchList` [ Localized (SourcePos "localFile" 1 1) $ GitIssue Github "guibou" "krank" 2,
                              Localized (SourcePos "localFile" 3 9) $ GitIssue (Gitlab (GitlabHost "gitlab.com")) "gitlab-org" "gitlab-foss" 67390,
                              Localized (SourcePos "localFile" 4 25) $ GitIssue Github "guibou" "krank" 1,
                              Localized (SourcePos "localFile" 5 16) $ GitIssue (Gitlab (GitlabHost "gitlab.haskell.org")) "ghc" "ghc" 16955
                            ]
  describe "huge test" $ it "should work" $ do
    let config =
          KrankConfig
            { githubKey = Nothing,
              gitlabKeys = Map.empty,
              dryRun = False,
              useColors = False
            }
        env =
          TestEnv
            { envFiles = Map.singleton "foo" " hello you https://github.com/foo/bar/issues/10 yeah\nhttps://github.com/foo/bar/issues/11",
              envRestAnswers =
                Map.fromList
                  [ (Req.https "api.github.com" Req./: "repos" Req./: "foo" Req./: "bar" Req./: "issues" Req./: "10", Right $ object [("state", String "closed")]),
                    (Req.https "api.github.com" Req./: "repos" Req./: "foo" Req./: "bar" Req./: "issues" Req./: "11", Right $ object [("state", String "open")])
                  ]
            }
    let Right res = runReaderT (runWriterT (unTestKrank $ runKrank ["foo", "bar"])) (env, config)
    res
      `shouldBe` ( (),
                   ( ["\nfoo:1:12: error:\n  IssueTracker check for https://github.com/foo/bar/issues/10\n    now Closed\n\nfoo:2:1: info:\n  IssueTracker check for https://github.com/foo/bar/issues/11\n    still Open\n"] :: [Text],
                     [ "Error when processing bar: user error (file not found)"
                     ] ::
                       [Text]
                   )
                 )
