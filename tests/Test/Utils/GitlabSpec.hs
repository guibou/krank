{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Leaving unnecessary do because
--  * they improve readability
--  * if you add a context, or an it, it'll still compile, without remembering that there was no do
--  before because at the time there was a single function inside
--  * without those do, ormolu does some formatting that looks crazy to me
{- Hlint ignore "Redundant do" -}

module Test.Utils.GitlabSpec
  ( spec,
  )
where

import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap (singleton)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text (Text, isInfixOf, isPrefixOf)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), createCookieJar, defaultRequest)
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..))
import Network.HTTP.Req (HttpException (..))
import Network.HTTP.Types (Status, status400, status403, status404)
import Network.HTTP.Types.Version (http11)
import Test.Hspec
import Utils.Gitlab (gitlabAPILimitErrorText, gitlabNotFoundErrorText, showGitlabException)
import Utils.Req (showHTTPException, showRawResponse)

spec :: Spec
spec = do
  describe "Utils.Gitlab" $ do
    context "Gitlab \"business\" errors, 4xx status code, \"message\" structure" $ do
      it "Display the content of the error message, deserialized from the JSON" $ do
        let errorMsg = "some_error"
        let body = mkGitlabErrorBody errorMsg
        let errorText = getErrorText body status400
        errorText `shouldSatisfy` isPrefixOf "From Gitlab:"
        errorText `shouldSatisfy` isInfixOf errorMsg
    context "Gitlab API Rate Limit error" $ do
      it "Displays the specific message for API errors" $ do
        let errorMsg = "API rate limit exceeded for 86.111.137.132. (But here's the good news: Authenticated requests get a higher rate limit. Check out the documentation for more details.)"
        let body = mkGitlabErrorBody errorMsg
        let errorText = getErrorText body status403
        errorText `shouldSatisfy` isInfixOf gitlabAPILimitErrorText
    context "Gitlab Private repo error" $ do
      it "Displays the specific message for private repo error errors" $ do
        let errorMsg = "Not Found"
        let body = mkGitlabErrorBody errorMsg
        let errorText = getErrorText body status404
        errorText `shouldSatisfy` isInfixOf gitlabNotFoundErrorText
    context "Non-JSON response exception" $ do
      it "Shows the raw exception, through the generic helper" $ do
        let exception = JsonHttpException "JSON decoding failed"
        showGitlabException exception `shouldBe` showHTTPException showRawResponse exception
    context "Invalid URL exception" $ do
      it "Shows the raw exception, through the generic helper" $ do
        let exception = VanillaHttpException $ InvalidUrlException "file://foo" "bar"
        showGitlabException exception `shouldBe` showHTTPException showRawResponse exception
    context "otherHttpException" $ do
      it "Shows the raw exception, through the generic helper" $ do
        let exception = VanillaHttpException $ HttpExceptionRequest defaultRequest ConnectionTimeout
        showGitlabException exception `shouldBe` showHTTPException showRawResponse exception

dummyResponse :: Status -> Response ()
#if MIN_VERSION_http_client(0,7,8)
dummyResponse status = Response status http11 [] () (createCookieJar []) (ResponseClose $ pure ()) (error "WTF")
#else
dummyResponse status = Response status http11 [] () (createCookieJar []) (ResponseClose $ pure ())
#endif

-- | From a raw error message to a JSON formatted gitlab error
mkGitlabErrorBody :: Text -> ByteString
mkGitlabErrorBody msg = encode $ Object $ singleton "message" (String msg)

-- | From a raw JSON response body and status code to the interpreted displayed text
getErrorText :: ByteString -> Status -> Text
getErrorText body status = showGitlabException $ VanillaHttpException $ HttpExceptionRequest defaultRequest (StatusCodeException (dummyResponse status) (toStrict body))
