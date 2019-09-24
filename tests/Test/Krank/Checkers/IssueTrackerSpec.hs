module Test.Krank.Checkers.IssueTrackerSpec (
  spec
  ) where

import Test.Hspec

import Text.Regex.Applicative ((=~))

import Krank.Checkers.IssueTracker

spec :: Spec
spec = do
  context "Test.Krank.Checkers.specIssueTracker" $ do
    describe "#githubRE" $ do
      it "handles full https url" $ do
        "https://github.com/guibou/krank/issues/1\n" =~ githubRE `shouldBe` Just 1

      it "handles full http url" $ do
        "http://github.com/guibou/krank/issues/1\n" =~ githubRE `shouldBe` Just 1

      it "handles short url - no protocol" $ do
        "github.com/guibou/krank/issues/1\n" =~ githubRE `shouldBe` Just 1

      it "accepts www in url" $ do
        "https://www.github.com/guibou/krank/issues/1\n" =~ githubRE `shouldBe` Just 1

      it "accepts www in url - no protocol" $ do
        "www.github.com/guibou/krank/issues/1\n" =~ githubRE `shouldBe` Just 1

      it "accepts trailing slash" $ do
        "github.com/guibou/krank/issues/1/\n" =~ githubRE `shouldBe` Just 1

      it "fails if the issue number is not an int" $ do
        "github.com/guibou/krank/issues/foo\n" =~ githubRE `shouldBe` Nothing

      it "fails if there are too many components in the path" $ do
        "github.com/guibou/krank/should_not_be_here/issues/1\n" =~ githubRE `shouldBe` Nothing

      it "fails if github not in path" $ do
        "google.com/guibou/krank/issues/1\n" =~ githubRE `shouldBe` Nothing

      it "fails if not a github issue" $ do
        "github.com/guibou/krank/branches/1\n" =~ githubRE `shouldBe` Nothing

      it "fails on partial match" $ do
        "github.com/guibou/krank/\n" =~ githubRE `shouldBe` Nothing

      it "fails on partial match (just missing the issue number)" $ do
        "github.com/guibou/krank/issues/\n" =~ githubRE `shouldBe` Nothing
