{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative (optional)
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>), many)

import System.Console.Pretty (supportsPretty)

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
      <> Opt.metavar "PERSONAL_GITHUB_KEY"
      <> Opt.help "A github developer key to allow for more API calls or access to private github repo for the IssueTracker checker"))

gitlabKeyToParse :: Opt.Parser (Maybe GitlabKey)
gitlabKeyToParse = optional (
  GitlabKey <$> (
    Opt.strOption $
      Opt.long "issuetracker-gitlabkey"
      <> Opt.metavar "PERSONAL_GITLAB_KEY"
      <> Opt.help "A gitlab developer key to allow reaching private gitlab repo for the IssueTracker checker"))

noColorParse :: Opt.Parser Bool
noColorParse = not <$> Opt.switch (
  Opt.long "no-colors"
    <> Opt.help "Disable colored outputs")

optionsParser :: Opt.Parser KrankOpts
optionsParser = KrankOpts
  <$> filesToParse
  <*> (KrankConfig
       <$> githubKeyToParse
       <*> gitlabKeyToParse
       <*> (Opt.switch $ Opt.long "dry-run"
        <> Opt.help "Perform a dry run. Parse file, but do not execute HTTP requests")
       <*> noColorParse
      )

opts :: Opt.ParserInfo KrankOpts
opts = Opt.info (optionsParser <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Checks the comments in FILES"
  <> Opt.header "krank - a comment linter / analytics tool" )

main :: IO ()
main = do
  canUseColor <- supportsPretty

  config <- Opt.execParser opts

  let
    kConfig = (krankConfig config) {
      useColors = useColors (krankConfig config) && canUseColor
      }

  runKrank (codeFilePaths config) kConfig
