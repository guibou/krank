import System.Environment (getArgs)
import System.Exit (exitFailure)

import Data.Semigroup ((<>))
import Data.Text (unpack)
import Options.Applicative (
  (<**>), Parser(), ParserInfo(), execParser, fullDesc, header, helper, info, progDesc
                           )

import Krank
import Krank.Formatter

main :: IO ()
main = do
  options <- execParser opts
  putStrLn "Program launched"
--   filePath <- parseArgs
--   case filePath of Just f -> putStrLn . unpack . showViolations $ processFile f
--                    Nothing -> printHelp >> exitFailure
--
-- parseArgs :: IO (Maybe FilePath)
-- parseArgs = do
--   args <- getArgs
--   case args of [a] -> pure $ Just a
--                _   -> pure Nothing
--
-- printHelp :: IO ()
-- printHelp = do
--   putStrLn "Usage: krank <filename>"
--   putStrLn ""
--   putStrLn "  filename: path to the file to check"
--   putStrLn ""
--   putStrLn "Example: krank app/Main.hs"

data Sample = Sample

sample :: Parser Sample
sample = pure Sample

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Checks the comments of a code file"
  <> header "krank - a comment linter / analytics tool" )
