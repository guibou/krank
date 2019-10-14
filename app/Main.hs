{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)

import System.IO (hPutStrLn, stderr)

import Data.Semigroup ((<>))
import Data.Text (unpack)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>), many)

import Control.Exception.Safe
import PyF

import Krank
import Krank.Formatter

data KrankOpts = KrankOpts {
  codeFilePaths :: [FilePath]
}

filesToParse :: Opt.Parser [FilePath]
filesToParse = many (Opt.argument Opt.str (Opt.metavar "FILES..."))

sample :: Opt.Parser KrankOpts
sample = KrankOpts <$> filesToParse

opts :: Opt.ParserInfo KrankOpts
opts = Opt.info (sample <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Checks the comments in FILES"
  <> Opt.header "krank - a comment linter / analytics tool" )

main :: IO ()
main = do
  options <- Opt.execParser opts
  (flip mapM_) (codeFilePaths options) $ \path -> do
    (processFile path >>= putStrLn . unpack . showViolations)
    `catchAnyDeep` (\(SomeException e) -> hPutStrLn stderr [fmt|Error when processing {path}: {show e}|])
