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

check :: ByteString -> Maybe GitIssueRef
check a = case extractIssuesOnALine a of
  [(_, x)] -> Just x
  _ -> Nothing

giturlTests :: GitServer -> Spec
giturlTests domain = do
  let domainName = serverDomain domain
  it "handles full https url" $ do
    let match = check [fmt|https://{domainName}/guibou/krank/issues/2|]
    match `shouldBe` Just (GitIssueRef domain "guibou" "krank" 2)
  it "handles full http url" $ do
    let match = check [fmt|http://{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssueRef domain "guibou" "krank" 1)
  it "handles short url - no protocol" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssueRef domain "guibou" "krank" 1)
  it "accepts www. in url" $ do
    let match = check [fmt|https://www.{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssueRef domain "guibou" "krank" 1)
  it "accepts www in url - no protocol" $ do
    let match = check [fmt|www.{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Just (GitIssueRef domain "guibou" "krank" 1)
  it "fails if the issue number is not an int" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/foo|]
    match `shouldBe` Nothing
  it "fails on partial match" $ do
    let match = check [fmt|{domainName}/guibou/krank/|]
    match `shouldBe` Nothing
  it "fails on partial match (just missing the issue number)" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/|]
    match `shouldBe` Nothing
  it "handles the odd /- in gitlab URL" $ do
    let match = check [fmt|{domainName}/guibou/krank/-/issues/3|]
    match `shouldBe` Just (GitIssueRef domain "guibou" "krank" 3)
  it "handles long gitlab url with groups" $ do
    let match = check [fmt|{domainName}/gbataille_main/sub_level_1/sub_level_2/deep_in_groups/issues/3|]
    match `shouldBe` Just (GitIssueRef domain "gbataille_main/sub_level_1/sub_level_2" "deep_in_groups" 3)
  it "handles long gitlab url with groups and with the odd /-" $ do
    let match = check [fmt|{domainName}/gbataille_main/sub_level_1/sub_level_2/deep_in_groups/-/issues/12|]
    match `shouldBe` Just (GitIssueRef domain "gbataille_main/sub_level_1/sub_level_2" "deep_in_groups" 12)

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
          `shouldMatchList` [ Localized (SourcePos "localFile" 1 1) $ GitIssueRef Github "guibou" "krank" 2,
                              Localized (SourcePos "localFile" 3 9) $ GitIssueRef (Gitlab (GitlabHost "gitlab.com")) "gitlab-org" "gitlab-foss" 67390,
                              Localized (SourcePos "localFile" 4 25) $ GitIssueRef Github "guibou" "krank" 1,
                              Localized (SourcePos "localFile" 5 16) $ GitIssueRef (Gitlab (GitlabHost "gitlab.haskell.org")) "ghc" "ghc" 16955
                            ]
  describe "huge test" $ do
    let config =
          KrankConfig
            { githubKey = Nothing,
              gitlabKeys = Map.empty,
              dryRun = False,
              useColors = False
            }
        env state10 title10 title11 =
          TestEnv
            { envFiles = Map.singleton "foo" " hello you https://github.com/foo/bar/issues/10 yeah\nhttps://github.com/foo/bar/issues/11",
              envRestAnswers =
                Map.fromList
                  [ (Req.https "api.github.com" Req./: "repos" Req./: "foo" Req./: "bar" Req./: "issues" Req./: "10", Right $ object [("state", String state10), ("title", String title10)]),
                    (Req.https "api.github.com" Req./: "repos" Req./: "foo" Req./: "bar" Req./: "issues" Req./: "11", Right $ object [("state", String "open"), ("title", String title11)])
                  ]
            }
    it "should work" $ do
      let firstIssueTitle = "fooobar"
      let secondIssueTitle = "barbaz"
      let Right res = runReaderT (runWriterT (unTestKrank $ runKrank ["foo", "bar"])) (env "closed" firstIssueTitle secondIssueTitle, config)
      res
        `shouldBe` ( False,
                     ( [[fmt|\nfoo:1:12: error:\n  IssueTracker check for https://github.com/foo/bar/issues/10\n    the issue is now Closed - You can remove the workaround you used there\n    | title: {firstIssueTitle}\n\nfoo:2:1: info:\n  IssueTracker check for https://github.com/foo/bar/issues/11\n    the issue is still Open\n    | title: {secondIssueTitle}\n|]] :: [Text],
                       ["Error when processing bar: user error (file not found)"] :: [Text]
                     )
                   )
    it "with error in url" $ do
      let firstIssueTitle = "barbouze"
      let secondIssueTitle = "bouya"
      let Right res = runReaderT (runWriterT (unTestKrank $ runKrank ["foo"])) (env "closed" firstIssueTitle secondIssueTitle, config)
      res
        `shouldBe` ( False,
                     ( [[fmt|\nfoo:1:12: error:\n  IssueTracker check for https://github.com/foo/bar/issues/10\n    the issue is now Closed - You can remove the workaround you used there\n    | title: {firstIssueTitle}\n\nfoo:2:1: info:\n  IssueTracker check for https://github.com/foo/bar/issues/11\n    the issue is still Open\n    | title: {secondIssueTitle}\n|]],
                       [] :: [Text]
                     )
                   )
    it "with error in file" $ do
      let firstIssueTitle = "yazu"
      let secondIssueTitle = "zuma"
      let Right res = runReaderT (runWriterT (unTestKrank $ runKrank ["foo", "bar"])) (env "open" firstIssueTitle secondIssueTitle, config)
      res
        `shouldBe` ( False,
                     ( [[fmt|\nfoo:1:12: info:\n  IssueTracker check for https://github.com/foo/bar/issues/10\n    the issue is still Open\n    | title: {firstIssueTitle}\n\nfoo:2:1: info:\n  IssueTracker check for https://github.com/foo/bar/issues/11\n    the issue is still Open\n    | title: {secondIssueTitle}\n|]] :: [Text],
                       [ "Error when processing bar: user error (file not found)"
                       ] ::
                         [Text]
                     )
                   )
    -- Note: it only tests the semantic of top level function.
    -- We need integratino tests wich ensures that the final function
    -- works correctly.
    -- Or we need to move more things inside the Krank monad, such as "exitFailure".
    it "without error" $ do
      let firstIssueTitle = "malaria"
      let secondIssueTitle = "ria riu rio"
      let Right res = runReaderT (runWriterT (unTestKrank $ runKrank ["foo"])) (env "open" firstIssueTitle secondIssueTitle, config)
      res
        `shouldBe` ( True,
                     ( [[fmt|\nfoo:1:12: info:\n  IssueTracker check for https://github.com/foo/bar/issues/10\n    the issue is still Open\n    | title: {firstIssueTitle}\n\nfoo:2:1: info:\n  IssueTracker check for https://github.com/foo/bar/issues/11\n    the issue is still Open\n    | title: {secondIssueTitle}\n|]] :: [Text],
                       [] :: [Text]
                     )
                   )
    it "ignore are ignored" $ do
      let firstIssueTitle = "rio del mare"
      let secondIssueTitle = "margulin"
      let testConfig =
            KrankConfig
              { githubKey = Nothing,
                gitlabKeys = Map.empty,
                dryRun = False,
                useColors = False
              }
          testEnv =
            TestEnv
              { envFiles = Map.singleton "foo" " hello you https://github.com/foo/bar/issues/10 yeah# krank:ignore-line\nhttps://github.com/foo/bar/issues/11",
                envRestAnswers =
                  Map.fromList
                    [ (Req.https "api.github.com" Req./: "repos" Req./: "foo" Req./: "bar" Req./: "issues" Req./: "10", Right $ object [("state", String "closed"), ("title", String firstIssueTitle)]),
                      (Req.https "api.github.com" Req./: "repos" Req./: "foo" Req./: "bar" Req./: "issues" Req./: "11", Right $ object [("state", String "open"), ("title", String secondIssueTitle)])
                    ]
              }
      let Right res = runReaderT (runWriterT (unTestKrank $ runKrank ["foo", "bar"])) (testEnv, testConfig)
      -- TODO: perhaps ignored lines must appears in the listing, but not as error
      res
        `shouldBe` ( False,
                     ( [[fmt|\nfoo:2:1: info:\n  IssueTracker check for https://github.com/foo/bar/issues/11\n    the issue is still Open\n    | title: {secondIssueTitle}\n|]] :: [Text],
                       [ "Error when processing bar: user error (file not found)"
                       ] ::
                         [Text]
                     )
                   )
  describe "it parses when there is two url on the same line" $ do
    it "works correctly with only one in second position" $ do
      extractIssues "foo" "https://ip.tyk.nu https://github.com/x/x/issues/32"
        `shouldBe` [ Localized (SourcePos "foo" 1 19) (GitIssueRef Github "x" "x" 32)
                   ]
    it "works correctly with only one in first position" $ do
      extractIssues "foo" "foo bar baz https://github.com/x/x/issues/32 https://ip.tyk.nu"
        `shouldBe` [ Localized (SourcePos "foo" 1 13) (GitIssueRef Github "x" "x" 32)
                   ]
    it "works correctly with two correct url" $ do
      extractIssues "foo" "foo gitlab.com/foo/br/issues/10 https://github.com/x/x/issues/32 https://ip.tyk.nu"
        `shouldBe` [ Localized (SourcePos "foo" 1 5) (GitIssueRef (Gitlab (GitlabHost "gitlab.com")) "foo" "br" 10),
                     Localized (SourcePos "foo" 1 33) (GitIssueRef Github "x" "x" 32)
                   ]
