{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Krank.Checkers.IssueTrackerSpec (
  spec
  ) where

import PyF (fmt)
import Test.Hspec

import Krank.Checkers.IssueTracker
import Text.Megaparsec (parseMaybe, Parsec)
import Data.Void
import Text.Megaparsec.Pos (SourcePos(..), mkPos)

-- | Alias for fast parsing
(=~) :: String -> Parsec Void String GitIssue -> Maybe GitIssue
url =~ parser = parseMaybe parser url

spec :: Spec
spec =
  context "Test.Krank.Checkers.specIssueTracker" $ do
    describe "#githubParser" $ do
      it "handles full https url" $ do
        let match = "https://github.com/guibou/krank/issues/1" =~ githubParser
        match `shouldBe` (Just $ GitIssue Github "guibou" "krank" 1)

      it "handles full http url" $ do
        let match = "http://github.com/guibou/krank/issues/1" =~ githubParser
        match `shouldBe` (Just $ GitIssue Github "guibou" "krank" 1)

      it "handles short url - no protocol" $ do
        let match = "github.com/guibou/krank/issues/1" =~ githubParser
        match `shouldBe` (Just $ GitIssue Github "guibou" "krank" 1)

      it "accepts www in url" $ do
        let match = "https://www.github.com/guibou/krank/issues/1" =~ githubParser
        match `shouldBe` (Just $ GitIssue Github "guibou" "krank" 1)

      it "accepts www in url - no protocol" $ do
        let match = "www.github.com/guibou/krank/issues/1" =~ githubParser
        match `shouldBe` (Just $ GitIssue Github "guibou" "krank" 1)

      it "fails if the issue number is not an int" $ do
        let match = "github.com/guibou/krank/issues/foo" =~ githubParser
        match `shouldBe` Nothing

      it "fails if there are too many components in the path" $ do
        let match = "github.com/guibou/krank/should_not_be_here/issues/1" =~ githubParser
        match `shouldBe` Nothing

      it "fails if github not in path" $ do
        let match = "google.com/guibou/krank/issues/1" =~ githubParser
        match `shouldBe` Nothing

      it "fails if not a github issue" $ do
        let match = "github.com/guibou/krank/branches/1" =~ githubParser
        match `shouldBe` Nothing

      it "fails on partial match" $ do
        let match = "github.com/guibou/krank/" =~ githubParser
        match `shouldBe` Nothing

      it "fails on partial match (just missing the issue number)" $ do
        let match = "github.com/guibou/krank/issues/" =~ githubParser
        match `shouldBe` Nothing

      it "handles full https url" $ do
        let match = "https://github.com/guibou/krank/issues/2" =~ githubParser
        match `shouldBe` (Just $ GitIssue Github "guibou" "krank" 2)

    describe "#extractIssues" $
      it "handles both github and gitlab" $ do
        let match = extractIssues "localFile" [fmt|https://github.com/guibou/krank/issues/2
        some text
        https://gitlab.com/gitlab-org/gitlab-foss/issues/67390
        and more github https://github.com/guibou/krank/issues/1
        |]
        match `shouldMatchList` [
          Localized (SourcePos "localFile" (mkPos 1) (mkPos 1)) $ GitIssue Github "guibou" "krank" 2
          , Localized (SourcePos "localFile" (mkPos 3) (mkPos 17)) $ GitIssue Gitlab "gitlab-org" "gitlab-foss" 67390
          , Localized (SourcePos "localFile" (mkPos 4) (mkPos 25)) $ GitIssue Github "guibou" "krank" 1 ]
