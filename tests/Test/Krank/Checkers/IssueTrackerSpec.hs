{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Krank.Checkers.IssueTrackerSpec (
  spec
  ) where

import PyF (fmt)
import Test.Hspec

import Krank.Checkers.IssueTracker
import Krank.Types
import Data.ByteString.Char8 (ByteString)

check :: ByteString -> Maybe GitIssue
check a = case extractIssuesOnALine a of
  [(_, x)] -> Just x
  _ -> Nothing

giturlTests :: GitServer -> Spec
giturlTests domain = do
  let
    domainName = serverDomain domain

  it "handles full https url" $ do
    let match = check [fmt|https://{domainName}/guibou/krank/issues/1|]
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "Does not wildcard . in domain name" $ do
    let match = check [fmt|https://{map replaceDot domainName}/guibou/krank/issues/1|]
        replaceDot '.' = 'X'
        replaceDot x = x
    match `shouldBe` Nothing

  it "handles full http url" $ do
    let match = check [fmt|http://{domainName}/guibou/krank/issues/1|]
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "handles short url - no protocol" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/1|]
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "accepts www. in url" $ do
    let match = check [fmt|https://www.{domainName}/guibou/krank/issues/1|]
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "refuses wwwX" $ do -- to avoid matching dots in url
    let match = check [fmt|https://wwwX{domainName}/guibou/krank/issues/1|]
    match `shouldBe` Nothing

  it "accepts www in url - no protocol" $ do
    let match = check [fmt|www.{domainName}/guibou/krank/issues/1|]
    match `shouldBe` (Just $ GitIssue domain "guibou" "krank" 1)

  it "fails if the issue number is not an int" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/foo|]
    match `shouldBe` Nothing

  it "fails if there are too many components in the path" $ do
    let match = check [fmt|{domainName}/guibou/krank/should_not_be_here/issues/1|]
    match `shouldBe` Nothing

  it "fails if github not in path" $ do
    let match = check [fmt|google.com/guibou/krank/issues/1|]
    match `shouldBe` Nothing

  it "fails if not a github issue" $ do
    let match = check [fmt|{domainName}/guibou/krank/branches/1|]
    match `shouldBe` Nothing

  it "fails on partial match" $ do
    let match = check [fmt|{domainName}/guibou/krank/|]
    match `shouldBe` Nothing

  it "fails on partial match (just missing the issue number)" $ do
    let match = check [fmt|{domainName}/guibou/krank/issues/|]
    match `shouldBe` Nothing

  it "handles full https url" $ do
    let match = check [fmt|https://{domainName}/guibou/krank/issues/2|]
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
          Localized (SourcePos "localFile" 1 1) $ GitIssue Github "guibou" "krank" 2
          , Localized (SourcePos "localFile" 3 9) $ GitIssue Gitlab "gitlab-org" "gitlab-foss" 67390
          , Localized (SourcePos "localFile" 4 25) $ GitIssue Github "guibou" "krank" 1 ]
