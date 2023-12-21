module Synapse.Syntax.Parser.Rule
  where

import Synapse.Syntax.Rule
import Synapse.Syntax.Judgment
import Synapse.Syntax.Parser.Context
import Synapse.Syntax.Parser.Utils
import Synapse.Syntax.Parser.Judgment

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Functor

parseQuery :: [JudgmentSpec] -> Parser Query
parseQuery specs = Query <$> parseSomeJudgment specs

parseRule :: [JudgmentSpec] -> Parser Rule
parseRule jSpecs = label "rule" . lexemeNewline $ do
  premises <- many (parseSomeJudgment jSpecs)
  parseHLine
  name <- optional parseRuleName
  conclusion <- parseSomeJudgment jSpecs
  pure $ Rule name premises conclusion

parseHLine :: Parser ()
parseHLine = label "horizontal line" . lexemeNewline $ do
  xs <- lexeme $ some (char '-')
  parserGuard (length xs > 3) Nothing "at least 3 -s"

parseRuleName :: Parser String
parseRuleName = label "rule name" . lexemeNewline $ do
  symbol "["
  name <- parseIdentifier
  symbol "]"
  pure name

