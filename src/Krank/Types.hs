{-# LANGUAGE DataKinds #-}

module Krank.Types
  ( GithubKey (..),
    GitlabHost (..),
    GitlabKey (..),
    Violation (..),
    ViolationLevel (..),
    KrankConfig (..),
    SourcePos (..),
    Localized (..),
    MonadKrank (..),
  )
where

import Control.Exception.Safe (MonadCatch)
import Data.Aeson (FromJSON)
import Data.ByteString
import Data.Map (Map)
import Data.Text (Text)
import qualified Network.HTTP.Req as Req

newtype GithubKey = GithubKey Text deriving (Show)

newtype GitlabKey = GitlabKey Text deriving (Show)

newtype GitlabHost = GitlabHost Text deriving (Show, Ord, Eq)

data ViolationLevel = Info | Warning | Error deriving (Show)

data SourcePos
  = SourcePos
      { file :: FilePath,
        lineNumber :: Int,
        colNumber :: Int
      }
  deriving (Show, Eq, Ord)

-- | Represents a localized chunk of information
-- in a file
data Localized t
  = Localized
      { getLocation :: SourcePos,
        unLocalized :: t
      }
  deriving (Show, Eq)

data Violation
  = Violation
      { -- | A textual representation of the checker. Most of the time that's
        -- the chunck of text parsed
        checker :: Text,
        -- | The 'ViolationLevel' associated with the result
        level :: ViolationLevel,
        -- | A message describing the error
        message :: Text,
        -- | The position in the input sources of the chunck
        location :: SourcePos
      }
  deriving (Show)

data KrankConfig
  = KrankConfig
      { -- | The github oAuth token
        githubKey :: Maybe GithubKey,
        -- | The gitlab oAuth token
        gitlabKeys :: Map GitlabHost GitlabKey,
        -- | If 'True', all IO operations, such as HTTP requests, are ignored
        dryRun :: Bool,
        -- | Use color for formatting
        useColors :: Bool
      }
  deriving (Show)

-- | This monad represents all the effect that Krank needs
class (Monad m, MonadCatch m) => MonadKrank m where
  -- | Run a REST requet
  krankRunRESTRequest :: FromJSON t => Req.Url 'Req.Https -> Req.Option 'Req.Https -> m t

  -- | Read the configuration
  krankAsks :: (KrankConfig -> b) -> m b

  -- * Concurrency

  -- | Apply a function on many item in a concurrent way
  krankMapConcurrently :: (a -> m b) -> [a] -> m [b]

  krankForConcurrently :: [a] -> (a -> m b) -> m [b]
  krankForConcurrently = flip krankMapConcurrently

  -- * IO Part

  -- | Read a file from filesystem
  krankReadFile :: FilePath -> m ByteString

  -- | Log an error (with trailing \n)
  krankPutStrLnStderr :: Text -> m ()

  -- | Log a message (without trailing \n)
  krankPutStr :: Text -> m ()
