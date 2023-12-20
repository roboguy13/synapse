module Synapse.Syntax.Parser.Judgment
  where

import Synapse.Syntax.Judgment
import Synapse.Syntax.Parser.Utils
import Synapse.Syntax.Parser.Term
import Synapse.Syntax.Term

import Text.Megaparsec

import Data.Maybe
import Data.Functor

parseJudgment :: [JudgmentSpec] -> Parser Judgment
parseJudgment [] = parserFailure Nothing "judgment"
parseJudgment (spec:specs) =
  try (parseFromJudgmentSpec spec) <|> parseJudgment specs

parseFromJudgmentSpec :: JudgmentSpec -> Parser Judgment
parseFromJudgmentSpec spec = label "judgment" . lexeme $ do
  ts <- catMaybes <$> traverse parseFromPart (judgmentSpecParts spec)
  pure $ Judgment spec ts

parseFromPart :: SpecPart -> Parser (Maybe Term)
parseFromPart ParamSpot = lexeme $ Just <$> parseTerm
parseFromPart (OperatorPart x) = lexeme $ keyword x $> Nothing

