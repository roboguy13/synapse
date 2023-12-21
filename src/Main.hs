module Main where

import Synapse.Syntax.System
import Synapse.Syntax.Parser.System
import Synapse.Ppr

import Text.Megaparsec

import System.Environment

main :: IO ()
main = do
  (fileName:_) <- getArgs
  fileText <- readFile fileName
  case parse parseSystem fileName fileText of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right r -> putStrLn $ "Parsed:\n" ++ show (ppr r)

