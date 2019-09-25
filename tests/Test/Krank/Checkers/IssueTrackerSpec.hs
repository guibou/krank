{-# LANGUAGE QuasiQuotes #-}

module Test.Krank.Checkers.IssueTrackerSpec (
  spec
  ) where

import PyF (fmt)
import Test.Hspec
import Text.Regex.Applicative ((=~))

import Krank.Checkers.IssueTracker

spec :: Spec
spec =
  context "Test.Krank.Checkers.specIssueTracker" $ do
    describe "#gitRepoRE" $ do
      it "handles full https url" $ do
        let match = "https://github.com/guibou/krank/issues/1" =~ gitRepoRE "github.com"
        match `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "handles full http url" $ do
        let match = "http://github.com/guibou/krank/issues/1" =~ gitRepoRE "github.com"
        match `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "handles short url - no protocol" $ do
        let match = "github.com/guibou/krank/issues/1" =~ gitRepoRE "github.com"
        match `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "accepts www in url" $ do
        let match = "https://www.github.com/guibou/krank/issues/1" =~ gitRepoRE "github.com"
        match `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "accepts www in url - no protocol" $ do
        let match = "www.github.com/guibou/krank/issues/1" =~ gitRepoRE "github.com"
        match `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "uses its parameter to match the host" $ do
        let host = "foobar.baz"
        let match = [fmt|https://www.{host}/guibou/krank/issues/1|] =~ gitRepoRE host
        match `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "fails if the issue number is not an int" $ do
        let match = "github.com/guibou/krank/issues/foo" =~ gitRepoRE "github.com"
        match `shouldBe` Nothing

      it "fails if there are too many components in the path" $ do
        let match = "github.com/guibou/krank/should_not_be_here/issues/1" =~ gitRepoRE "github.com"
        match `shouldBe` Nothing

      it "fails if github not in path" $ do
        let match = "google.com/guibou/krank/issues/1" =~ gitRepoRE "github.com"
        match `shouldBe` Nothing

      it "fails if not a github issue" $ do
        let match = "github.com/guibou/krank/branches/1" =~ gitRepoRE "github.com"
        match `shouldBe` Nothing

      it "fails on partial match" $ do
        let match = "github.com/guibou/krank/" =~ gitRepoRE "github.com"
        match `shouldBe` Nothing

      it "fails on partial match (just missing the issue number)" $ do
        let match = "github.com/guibou/krank/issues/" =~ gitRepoRE "github.com"
        match `shouldBe` Nothing

    describe "#gitlabRE" $
      it "handles full https url" $ do
        let match = "https://gitlab.com/gitlab-org/gitlab-foss/issues/67390" =~ gitlabRE
        match `shouldBe` (Just $ GitIssue "gitlab-org" "gitlab-foss" 67390)

    describe "#githubRE" $
      it "handles full https url" $ do
        let match = "https://github.com/guibou/krank/issues/2" =~ githubRE
        match `shouldBe` (Just $ GitIssue "guibou" "krank" 2)

    describe "#extractIssues" $
      it "handles both github and gitlab" $ do
        let match = extractIssues [fmt|https://github.com/guibou/krank/issues/2
        some text
        https://gitlab.com/gitlab-org/gitlab-foss/issues/67390
        and more github https://github.com/guibou/krank/issues/1
        |]
        match `shouldMatchList` [
          GitIssue "guibou" "krank" 2
          , GitIssue "gitlab-org" "gitlab-foss" 67390
          , GitIssue "guibou" "krank" 1 ]
