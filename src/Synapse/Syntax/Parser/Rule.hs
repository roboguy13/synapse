module Synapse.Syntax.Parser.Rule
  where

import Synapse.Syntax.Rule
import Synapse.Syntax.Judgment
import Synapse.Syntax.Parser.Utils
import Synapse.Syntax.Parser.Judgment

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Functor

parseRule :: [JudgmentSpec] -> Parser Rule
parseRule jSpecs = label "rule" . lexeme $ do
  premises <- many (parseJudgment jSpecs <* newline)
  parseHLine
  name <- optional parseRuleName
  newline
  conclusion <- parseJudgment jSpecs
  pure $ Rule name premises conclusion

parseHLine :: Parser ()
parseHLine = label "horizontal line" . lexeme $ do
  xs <- some (char '-')
  parserGuard (length xs > 3) Nothing "at least 3 -s"

parseRuleName :: Parser String
parseRuleName = label "rule name" . lexeme $ do
  symbol "["
  name <- parseIdentifier
  symbol "]"
  pure name

