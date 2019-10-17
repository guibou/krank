{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Control.Exception.Safe
import Control.Applicative (optional)
import Data.Semigroup ((<>))
import Data.Text (unpack)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>), many)
import PyF

import Krank
import Krank.Formatter

data KrankOpts = KrankOpts {
  codeFilePaths :: [FilePath],
  githubKey :: Maybe String
}

filesToParse :: Opt.Parser [FilePath]
filesToParse = many (Opt.argument Opt.str (Opt.metavar "FILES..."))

githubKeyToParse :: Opt.Parser (Maybe String)
githubKeyToParse = optional $ Opt.strOption $
  Opt.long "github-key"
  <> Opt.metavar "DEVELOPER_KEY"
  <> Opt.help "A github developer key to allow for more API calls"

sample :: Opt.Parser KrankOpts
sample = KrankOpts
  <$> filesToParse
  <*> githubKeyToParse

opts :: Opt.ParserInfo KrankOpts
opts = Opt.info (sample <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Checks the comments in FILES"
  <> Opt.header "krank - a comment linter / analytics tool" )

main :: IO ()
main = do
  options <- Opt.execParser opts
  putStrLn $ show (githubKey options)
  (flip mapM_) (codeFilePaths options) $ \path -> do
    (processFile path >>= putStrLn . unpack . showViolations)
    `catchAnyDeep` (\(SomeException e) -> hPutStrLn stderr [fmt|Error when processing {path}: {show e}|])
