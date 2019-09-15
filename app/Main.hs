import System.Environment (getArgs)
import System.Exit (exitFailure)

import Krank

main :: IO ()
main = do
  filePath <- parseArgs
  case filePath of Just f -> putStrLn . show $ processFile f
                   Nothing -> printHelp >> exitFailure

parseArgs :: IO (Maybe FilePath)
parseArgs = do
  args <- getArgs
  case args of [a] -> pure $ Just a
               _   -> pure $ Nothing

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: krank <filename>"
  putStrLn ""
  putStrLn "  filename: path to the file to check"
  putStrLn ""
  putStrLn "Example: krank app/Main.hs"
