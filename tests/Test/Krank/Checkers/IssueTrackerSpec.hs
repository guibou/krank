module Test.Krank.Checkers.IssueTrackerSpec (
  spec
  ) where

import Test.Hspec

import Text.Regex.Applicative ((=~))

import Krank.Checkers.IssueTracker

spec :: Spec
spec =
  context "Test.Krank.Checkers.specIssueTracker" $
    describe "#githubRE" $ do
      it "handles full https url" $
        "https://github.com/guibou/krank/issues/1" =~ githubRE `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "handles full http url" $
        "http://github.com/guibou/krank/issues/1" =~ githubRE `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "handles short url - no protocol" $
        "github.com/guibou/krank/issues/1" =~ githubRE `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "accepts www in url" $
        "https://www.github.com/guibou/krank/issues/1" =~ githubRE `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "accepts www in url - no protocol" $
        "www.github.com/guibou/krank/issues/1" =~ githubRE `shouldBe` (Just $ GitIssue "guibou" "krank" 1)

      it "fails if the issue number is not an int" $
        "github.com/guibou/krank/issues/foo" =~ githubRE `shouldBe` Nothing

      it "fails if there are too many components in the path" $
        "github.com/guibou/krank/should_not_be_here/issues/1" =~ githubRE `shouldBe` Nothing

      it "fails if github not in path" $
        "google.com/guibou/krank/issues/1" =~ githubRE `shouldBe` Nothing

      it "fails if not a github issue" $
        "github.com/guibou/krank/branches/1" =~ githubRE `shouldBe` Nothing

      it "fails on partial match" $
        "github.com/guibou/krank/" =~ githubRE `shouldBe` Nothing

      it "fails on partial match (just missing the issue number)" $
        "github.com/guibou/krank/issues/" =~ githubRE `shouldBe` Nothing
