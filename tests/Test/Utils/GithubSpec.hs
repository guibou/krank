{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Leaving unnecessary do because
--  * they improve readability
--  * if you add a context, or an it, it'll still compile, without remembering that there was no do
--  before because at the time there was a single function inside
--  * without those do, ormolu does some formatting that looks crazy to me
{- Hlint ignore "Redundant do" -}

module Test.Utils.GithubSpec
  ( spec,
  )
where

import Data.Aeson (Value (..), encode)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.HashMap.Strict (singleton)
import Data.Text (Text, isInfixOf, isPrefixOf)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), createCookieJar, defaultRequest)
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..))
import Network.HTTP.Req (HttpException (..))
import Network.HTTP.Types (status400)
import Network.HTTP.Types.Version (http11)
import Test.Hspec
import Utils.Github (githubAPILimitErrorText, showGithubException)
import Utils.Req (showHTTPException, showRawResponse)

spec :: Spec
spec = do
  describe "Utils.Github" $ do
    context "Github \"business\" errors, 4xx status code, \"message\" structure" $ do
      it "Display the content of the error message, deserialized from the JSON" $ do
        let errorMsg = "some_error"
        let body = mkGithubErrorBody errorMsg
        let errorText = getErrorText body
        ("From Github:" `isPrefixOf` errorText) `shouldBe` True
        (errorMsg `isInfixOf` errorText) `shouldBe` True
    context "Github API Rate Limit error" $ do
      it "Displays the specific message for API errors" $ do
        let errorMsg = "API rate limit exceeded for 86.111.137.132. (But here's the good news: Authenticated requests get a higher rate limit. Check out the documentation for more details.)"
        let body = mkGithubErrorBody errorMsg
        let errorText = getErrorText body
        (githubAPILimitErrorText `isInfixOf` errorText) `shouldBe` True
    context "Non-JSON response exception" $ do
      it "Shows the raw exception, through the generic helper" $ do
        let exception = JsonHttpException "JSON decoding failed"
        showGithubException exception `shouldBe` showHTTPException showRawResponse exception
    context "Invalid URL exception" $ do
      it "Shows the raw exception, through the generic helper" $ do
        let exception = VanillaHttpException $ InvalidUrlException "file://foo" "bar"
        showGithubException exception `shouldBe` showHTTPException showRawResponse exception
    context "otherHttpException" $ do
      it "Shows the raw exception, through the generic helper" $ do
        let exception = VanillaHttpException $ HttpExceptionRequest defaultRequest ConnectionTimeout
        showGithubException exception `shouldBe` showHTTPException showRawResponse exception

dummyResponse :: Response ()
dummyResponse = Response status400 http11 [] () (createCookieJar []) (ResponseClose $ pure ())

-- | From a raw error message to a JSON formatted github error
mkGithubErrorBody :: Text -> ByteString
mkGithubErrorBody msg = encode $ Object $ singleton "message" (String msg)

-- | From a raw JSON response body to the interpreted displayed text
getErrorText :: ByteString -> Text
getErrorText body = showGithubException $ VanillaHttpException $ HttpExceptionRequest defaultRequest (StatusCodeException dummyResponse (toStrict body))
