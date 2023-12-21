module Synapse.Syntax.Parser.Context
  where

import Synapse.Syntax.Context
import Synapse.Syntax.Judgment
import Synapse.Syntax.Parser.Utils
import Synapse.Syntax.Parser.Term (parseVarName)
import Synapse.Syntax.Parser.Judgment

import Unbound.Generics.LocallyNameless

import Data.Functor

import Text.Megaparsec

parseSomeJudgment :: [JudgmentSpec] -> Parser SomeJudgment
parseSomeJudgment specs =
  try (fmap SomeHypJudgment (parseHypJudgment specs))
  <|>
  try (fmap SomeBasicJudgment (parseJudgmentNewline specs))

parseHypJudgment :: [JudgmentSpec] -> Parser HypJudgment
parseHypJudgment specs = label "hypothetical judgment" . lexemeNewline $ do
  ctx <- parseContext specs
  symbol "|-"
  body <- parseJudgment specs
  pure $ HypJudgment ctx body

parseContext :: [JudgmentSpec] -> Parser Context
parseContext specs = label "context" $
  try (parseExtend specs)
  <|>
  try parseContext'

parseContext' :: Parser Context
parseContext' = 
  try parseEmpty
  <|>
  try parseCtxVar

parseEmpty :: Parser Context
parseEmpty = lexeme $ keyword "<>" $> Empty

parseCtxVar :: Parser Context
parseCtxVar = lexeme $ CtxVar . string2Name <$> parseVarName

parseExtend :: [JudgmentSpec] -> Parser Context
parseExtend specs = lexeme $ do
  ctx <- parseContext'
  symbol ","
  j <- parseJudgment specs
  pure $ Extend ctx j

