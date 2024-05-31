module Main where

-- base
import Control.Monad
  ( unless )
import Data.Char
  ( isSpace )
import Data.List
  ( dropWhileEnd )
import System.Environment
  ( getArgs )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map

-- directory
import System.Directory
  ( doesFileExist )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn $ "Starting reverso!"
  -- Obtain input/output file paths from arguments to the preprocessor.
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      inputFileExists <- doesFileExist inputFile
      unless inputFileExists $
        error $
          unlines
            [ "Reverso could not read input file."
            , "Input file: " ++ inputFile ]
      -- Read the input file, reverse each line, and write them to the output.
      inputLines <- lines <$> readFile inputFile
      let outputLines = map reverse inputLines
      writeFile outputFile ( unlines outputLines )
    [ ver ]
      | ver `elem` [ "-v", "--version" ]
      -> putStrLn $ "Reverso, version " ++ reversoVersion
      | ver `elem` [ "--numeric-version" ]
      -> putStrLn reversoVersion
    _ -> do
      let s = if length args == 1
              then ""
              else "s"
      error $ unlines $
        ( "Reverso could not handle the argument" ++ s ++ ":" )
        : args

reversoVersion :: String
reversoVersion = "1.1"
