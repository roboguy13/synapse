{-# LANGUAGE LambdaCase #-}
module Synapse.Syntax.Parser.Judgment
  where

import Synapse.Syntax.Judgment
import Synapse.Syntax.Parser.Utils
import Synapse.Syntax.Parser.Term
import Synapse.Syntax.Term

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Maybe
import Data.Functor

parseJudgment :: [JudgmentSpec] -> Parser Judgment
parseJudgment = lexemeNewline .
  \case
    [] -> parserFailure Nothing "judgment"
    (spec:specs) ->
      try (parseFromJudgmentSpec spec) <|> parseJudgment specs

parseFromJudgmentSpec :: JudgmentSpec -> Parser Judgment
parseFromJudgmentSpec spec = label "judgment" . lexeme $ do
  ts <- catMaybes <$> traverse parseFromPart (judgmentSpecParts spec)
  pure $ Judgment spec ts

parseFromPart :: SpecPart -> Parser (Maybe Term)
parseFromPart (ParamSpot _) = lexeme $ Just <$> parseTerm
parseFromPart (OperatorPart x) = lexeme $ keyword x $> Nothing

-- | mix _ fix _ operator
parseJudgmentForm :: Parser [SpecPart]
parseJudgmentForm = some (lexeme parseSpecPart)

parseSpecPart :: Parser SpecPart
parseSpecPart =
  try (symbol "_" $> ParamSpot ()) <|>
  try (fmap OperatorPart parseJudgmentIdentifier)

parseJudgmentSpec :: Grammar -> Parser JudgmentSpec
parseJudgmentSpec grammar = lexemeNewline $ do
  form <- lexemeNewline parseJudgmentForm
  arity <- lexemeNewline $ parseJudgmentArity grammar form
  pure $ JudgmentSpec form arity

parseJudgmentArity :: Grammar -> [SpecPart] -> Parser [TermSpec]
parseJudgmentArity grammar [] = pure []
parseJudgmentArity grammar (OperatorPart str : parts) =
  symbol str *> parseJudgmentArity grammar parts
parseJudgmentArity grammar (ParamSpot _ : parts) = do
  var <- lexeme parseVarName
  let termSpec = lookupTermSpec grammar var
  fmap (termSpec:) (parseJudgmentArity grammar parts)

