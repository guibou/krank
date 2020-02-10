{-# LANGUAGE DataKinds #-}
module Krank.Types ( GithubKey(..)
                   , GitlabHost(..)
                   , GitlabKey(..)
                   , Violation(..)
                   , ViolationLevel(..)
                   , KrankConfig(..)
                   , SourcePos(..)
                   , Localized(..)
  ) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Network.HTTP.Req as Req
import Data.Aeson (Value)

newtype GithubKey = GithubKey Text deriving (Show)
newtype GitlabKey = GitlabKey Text deriving (Show)
newtype GitlabHost = GitlabHost Text deriving (Show, Ord, Eq)

data ViolationLevel = Info | Warning | Error deriving (Show)
data SourcePos = SourcePos { file :: FilePath
                           , lineNumber :: Int
                           , colNumber :: Int
                           } deriving (Show, Eq, Ord)

-- | Represents a localized chunk of information
-- in a file
data Localized t = Localized
  { getLocation :: SourcePos
  , unLocalized :: t
  } deriving (Show, Eq)

data Violation = Violation { checker :: Text
                             -- ^ A textual representation of the checker. Most of the time that's
                             -- the chunck of text parsed
                           , level :: ViolationLevel
                           -- ^ The 'ViolationLevel' associated with the result
                           , message :: Text
                           -- ^ A message describing the error
                           , location :: SourcePos
                           -- ^ The position in the input sources of the chunck
                           } deriving (Show)

data KrankConfig = KrankConfig
  { githubKey :: Maybe GithubKey
  -- ^ The github oAuth token
  , gitlabKeys :: Map GitlabHost GitlabKey
  -- ^ The gitlab oAuth token
  , dryRun :: Bool
  -- ^ If 'True', all IO operations, such as HTTP requests, are ignored
  , useColors :: Bool
  -- ^ Use color for formatting
  , runRESTRequest :: Req.Url 'Req.Https -> Req.Option 'Req.Https -> IO Value
  -- ^ The function used by Krank to run REST request. Mostly editable so it can be used in pure tests.
  }
