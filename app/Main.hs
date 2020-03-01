{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative (optional)
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Text as Text
import Krank
import Krank.Types
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>), many, some)
import PyF (fmt)
import System.Console.Pretty (supportsPretty)
import Text.Regex.PCRE.Heavy

data KrankOpts
  = KrankOpts
      { codeFilePaths :: [FilePath],
        krankConfig :: KrankConfig
      }

filesToParse :: Opt.Parser [FilePath]
filesToParse = some (Opt.argument Opt.str (Opt.metavar "FILES..."))

githubKeyToParse :: Opt.Parser (Maybe GithubKey)
githubKeyToParse =
  optional
    ( GithubKey
        <$> Opt.strOption
          ( Opt.long "issuetracker-githubkey"
              <> Opt.metavar "PERSONAL_GITHUB_KEY"
              <> Opt.help "A github developer key to allow for more API calls or access to private github repo for the IssueTracker checker"
          )
    )

parseGitlabKey :: Opt.ReadM (GitlabHost, GitlabKey)
parseGitlabKey = Opt.eitherReader $ \(Text.pack -> s) -> case scan [re|^([^=]+)=(.+)$|] s of
  [(_, [x, y])] -> Right (GitlabHost x, GitlabKey y)
  _ -> Left [fmt|Unable to parse gitlab key=value from: {s}|]

gitlabKeyToParse :: Opt.Parser (Map.Map GitlabHost GitlabKey)
gitlabKeyToParse =
  Map.fromList
    <$> many
      ( Opt.option parseGitlabKey $
          Opt.long "issuetracker-gitlabhost"
            <> Opt.metavar "HOST=PERSONAL_GITLAB_KEY"
            <> Opt.help "A couple of gitlab host and developer key to allow reaching private gitlab repo for the IssueTracker checker. Can be specified multiple times."
      )

noColorParse :: Opt.Parser Bool
noColorParse =
  not
    <$> Opt.switch
      ( Opt.long "no-colors"
          <> Opt.help "Disable colored outputs"
      )

optionsParser :: Opt.Parser KrankOpts
optionsParser =
  KrankOpts
    <$> filesToParse
    <*> ( KrankConfig
            <$> githubKeyToParse
            <*> gitlabKeyToParse
            <*> Opt.switch
              ( Opt.long "dry-run"
                  <> Opt.help "Perform a dry run. Parse file, but do not execute HTTP requests"
              )
            <*> noColorParse
        )

opts :: Opt.ParserInfo KrankOpts
opts =
  Opt.info
    (optionsParser <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Checks the comments in FILES"
        <> Opt.header "krank - a comment linter / analytics tool"
    )

main :: IO ()
main = do
  canUseColor <- supportsPretty
  config <- Opt.execParser opts
  let kConfig =
        (krankConfig config)
          { useColors = useColors (krankConfig config) && canUseColor
          }
  runReaderT (unKrank $ runKrank (codeFilePaths config)) kConfig
