{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Krank
  ( runKrank,
    Krank (..),
  )
where

import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Exception.Safe
import Control.Monad.Reader
import qualified Data.ByteString
import Data.Coerce
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Krank.Checkers.Ignore (filterViolations)
import qualified Krank.Checkers.IssueTracker as IT
import Krank.Formatter
import Krank.Types
import qualified Network.HTTP.Req as Req
import PyF
import System.IO (stderr)

processFile ::
  MonadKrank m =>
  -- | the file to analyze
  FilePath ->
  m [Violation]
processFile filePath = do
  content <- krankReadFile filePath
  violations <- IT.checkText filePath content
  let filtered = filterViolations violations filePath content
  -- forcing 'violations' to WHNF forces more of the processing to happen inside the thread and
  -- improves a bit the runtime performances in parallel.
  -- forcing to Normal Form (with deepseq) does not bring anymore improvement
  pure $! filtered

runKrank :: MonadKrank m => [FilePath] -> m Bool
runKrank paths = do
  KrankConfig {useColors} <- krankAsks id
  res <- krankForConcurrently paths $ \path ->
    (Right <$> processFile path)
      `catchAny` (\(SomeException e) -> pure $ Left [fmt|Error when processing {path}: {show e}|])
  forM_ res $ \case
    Left err -> krankPutStrLnStderr err
    Right violations -> krankPutStr (foldMap (showViolation useColors) violations)
  -- Check if any violation is an error
  pure $ all (not . isError) res

-- | Returns 'True' if any violation level is error or if any error occurs.
isError :: Either Text.Text [Violation] -> Bool
isError (Left _) = True
isError (Right violations) = any isViolationError violations

isViolationError :: Violation -> Bool
isViolationError Violation {level = Error} = True
isViolationError _ = False

-- | This just exists to avoid the orphan instance on MonadKrank
newtype Krank t = Krank {unKrank :: ReaderT KrankConfig IO t}
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadThrow)

-- | The real monad implementation for Krank
instance MonadKrank Krank where

  krankReadFile = Krank . liftIO . Data.ByteString.readFile

  krankAsks = Krank . asks

  krankPutStrLnStderr = Krank . liftIO . Text.IO.hPutStrLn stderr

  krankPutStr = Krank . liftIO . Text.IO.putStr

  -- Use threads for concurrency
  krankMapConcurrently f l = Krank $ mapConcurrently (coerce . f) l

  -- This implements a Req REST request
  krankRunRESTRequest url headers = Krank
    $ Req.runReq Req.defaultHttpConfig
    $ do
      r <-
        Req.req
          Req.GET
          url
          Req.NoReqBody
          Req.jsonResponse
          ( Req.header "User-Agent" "krank"
              <> headers
          )
      pure $ Req.responseBody r
