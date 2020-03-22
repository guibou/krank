{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Network.HTTP.Types (Status, status400, status403, status404)
import Network.HTTP.Types.Version (http11)
import Test.Hspec
import Utils.Github (githubAPILimitErrorText, githubNotFoundErrorText, showGithubException)
import Utils.Req (showHTTPException, showRawResponse)

spec :: Spec
spec = do
  describe "Utils.Github" $ do
    context "Github \"business\" errors, 4xx status code, \"message\" structure" $ do
      it "Display the content of the error message, deserialized from the JSON" $ do
        let errorMsg = "some_error"
        let body = mkGithubErrorBody errorMsg
        let errorText = getErrorText body status400
        errorText `shouldSatisfy` isPrefixOf "From Github:"
        errorText `shouldSatisfy` isInfixOf errorMsg
    context "Github API Rate Limit error" $ do
      it "Displays the specific message for API errors" $ do
        let errorMsg = "API rate limit exceeded for 86.111.137.132. (But here's the good news: Authenticated requests get a higher rate limit. Check out the documentation for more details.)"
        let body = mkGithubErrorBody errorMsg
        let errorText = getErrorText body status403
        errorText `shouldSatisfy` isInfixOf githubAPILimitErrorText
    context "Github Private repo error" $ do
      it "Displays the specific message for private repo error errors" $ do
        let errorMsg = "Not Found"
        let body = mkGithubErrorBody errorMsg
        let errorText = getErrorText body status404
        errorText `shouldSatisfy` isInfixOf githubNotFoundErrorText
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

dummyResponse :: Status -> Response ()
dummyResponse status = Response status http11 [] () (createCookieJar []) (ResponseClose $ pure ())

-- | From a raw error message to a JSON formatted github error
mkGithubErrorBody :: Text -> ByteString
mkGithubErrorBody msg = encode $ Object $ singleton "message" (String msg)

-- | From a raw JSON response body and status code to the interpreted displayed text
getErrorText :: ByteString -> Status -> Text
getErrorText body status = showGithubException $ VanillaHttpException $ HttpExceptionRequest defaultRequest (StatusCodeException (dummyResponse status) (toStrict body))
