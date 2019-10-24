{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative (optional)
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>), many)

import Krank
import Krank.Types

data KrankOpts = KrankOpts {
  codeFilePaths :: [FilePath],
  krankConfig :: KrankConfig
}

filesToParse :: Opt.Parser [FilePath]
filesToParse = many (Opt.argument Opt.str (Opt.metavar "FILES..."))

githubKeyToParse :: Opt.Parser (Maybe GithubKey)
githubKeyToParse = optional (
  GithubKey <$> (
    Opt.strOption $
      Opt.long "issuetracker-githubkey"
      <> Opt.metavar "DEVELOPER_KEY"
      <> Opt.help "A github developer key to allow for more API calls for the IssueTracker checker"))

optionsParser :: Opt.Parser KrankOpts
optionsParser = KrankOpts
  <$> filesToParse
  <*> (KrankConfig
       <$> githubKeyToParse
       <*> (Opt.switch $ Opt.long "dry-run"
        <> Opt.help "Perform a dry run. Parse file, but do not execute HTTP requests")
      )

opts :: Opt.ParserInfo KrankOpts
opts = Opt.info (optionsParser <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Checks the comments in FILES"
  <> Opt.header "krank - a comment linter / analytics tool" )

main :: IO ()
main = do
  config <- Opt.execParser opts

  runKrank (codeFilePaths config) (krankConfig config)
