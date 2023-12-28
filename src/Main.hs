module Main where

import Synapse.Syntax.System
import Synapse.Syntax.Rule
import Synapse.Syntax.Parser.System
import Synapse.Logic.Search
import Synapse.Ppr

import Text.Megaparsec

import System.Environment

main :: IO ()
main = do
  (fileName:_) <- getArgs
  fileText <- readFile fileName
  case parse parseSystem fileName fileText of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err

    Right sys -> do
      putStrLn $ "Parsed:\n" ++ show (ppr sys)
      putStrLn "==="
      mapM_ (print . runQuery (systemRules sys)) (systemQueries sys)
      -- mapM_ (print . queryResultDerivation . head . runQuery (systemRules sys)) (systemQueries sys)

