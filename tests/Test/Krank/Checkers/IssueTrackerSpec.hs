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

giturlTests :: GitServer -> Spec
giturlTests domain = do
  let
    domainName = serverDomain domain

  it "handles full https url" $ do
    let match  = [fmt|https://{domainName}/guibou/krank/issues/1|] =~ gitRepoParser
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "handles full http url" $ do
    let match  = [fmt|http://{domainName}/guibou/krank/issues/1|] =~ gitRepoParser
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "handles short url - no protocol" $ do
    let match  = [fmt|{domainName}/guibou/krank/issues/1|] =~ gitRepoParser
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "accepts www in url" $ do
    let match  = [fmt|https://www.{domainName}/guibou/krank/issues/1|] =~ gitRepoParser
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "accepts www in url - no protocol" $ do
    let match  = [fmt|www.{domainName}/guibou/krank/issues/1|] =~ gitRepoParser
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "fails if the issue number is not an int" $ do
    let match  = [fmt|{domainName}/guibou/krank/issues/foo|] =~ gitRepoParser
    match `shouldBe` Nothing

  it "fails if there are too many components in the path" $ do
    let match  = [fmt|{domainName}/guibou/krank/should_not_be_here/issues/1|] =~ gitRepoParser
    match `shouldBe` Nothing

  it "fails if github not in path" $ do
    let match  = [fmt|google.com/guibou/krank/issues/1|] =~ gitRepoParser
    match `shouldBe` Nothing

  it "fails if not a github issue" $ do
    let match  = [fmt|{domainName}/guibou/krank/branches/1|] =~ gitRepoParser
    match `shouldBe` Nothing

  it "fails on partial match" $ do
    let match  = [fmt|{domainName}/guibou/krank/|] =~ gitRepoParser
    match `shouldBe` Nothing

  it "fails on partial match (just missing the issue number)" $ do
    let match  = [fmt|{domainName}/guibou/krank/issues/|] =~ gitRepoParser
    match `shouldBe` Nothing

  it "handles full https url" $ do
    let match  = [fmt|https://{domainName}/guibou/krank/issues/2|] =~ gitRepoParser
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 2)

spec :: Spec
spec =
  context "Test.Krank.Checkers.specIssueTracker" $ do
    describe "#githubParser" $ do
      giturlTests Github

    describe "#githlabParser" $ do
      giturlTests Gitlab

    describe "#extractIssues" $
      it "handles both github and gitlab" $ do
        let match = extractIssues "localFile" [fmt|https://github.com/guibou/krank/issues/2
        some text
        https://gitlab.com/gitlab-org/gitlab-foss/issues/67390
        and more github https://github.com/guibou/krank/issues/1
        |]
        match `shouldMatchList` [
          Localized (SourcePos "localFile" (mkPos 1) (mkPos 1)) $ GitIssue Github "guibou" "krank" 2
          , Localized (SourcePos "localFile" (mkPos 3) (mkPos 9)) $ GitIssue Gitlab "gitlab-org" "gitlab-foss" 67390
          , Localized (SourcePos "localFile" (mkPos 4) (mkPos 25)) $ GitIssue Github "guibou" "krank" 1 ]
