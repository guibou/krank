import System.Environment (getArgs)
import System.Exit (exitFailure)

import Data.Semigroup ((<>))
import Data.Text (unpack)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))

import Krank
import Krank.Formatter

data Sample = Sample {
  codeFilePath :: FilePath
}

fileToParse :: Opt.Parser FilePath
fileToParse = Opt.argument Opt.str (Opt.metavar "FILE")

sample :: Opt.Parser Sample
sample = Sample
  <$> fileToParse

opts :: Opt.ParserInfo Sample
opts = Opt.info (sample <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Checks the comments in FILE"
  <> Opt.header "krank - a comment linter / analytics tool" )

main :: IO ()
main = do
  options <- Opt.execParser opts
  putStrLn . unpack . showViolations . processFile . codeFilePath $ options
