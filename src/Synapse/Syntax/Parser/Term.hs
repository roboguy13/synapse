module Synapse.Syntax.Parser.Term
  where

import Synapse.Syntax.Term
import Synapse.Syntax.Parser.Utils
import Synapse.Logic.Substitution

import Unbound.Generics.LocallyNameless

import Text.Megaparsec
import Text.Megaparsec.Char

parseSubstTerm :: Parser SubstTerm
parseSubstTerm =
  try parseSubst <|>
  fmap retagTerm parseTerm

parseSubst :: Parser SubstTerm
parseSubst = label "explicit substitution" $ lexeme $ do
  t <- parseTerm
  symbol "["
  xs <- parseOneSubst `sepBy1` symbol ","
  symbol "]"
  pure $ TSubst (retagTerm t) (mconcat xs)
  where
    parseOneSubst = lexeme $ do
      x <- lexeme parseVarName
      symbol ":="
      t <- parseTerm
      pure $ oneSubst (string2Name x) t

parseTerm :: Parser Term
parseTerm = label "term" $
  try parseVar <|> try parseIntLit <|> try parseSymbol <|> try parseApp

parseVar :: Parser Term
parseVar = Var . string2Name <$> lexeme parseVarName

parseSymbol :: Parser Term
parseSymbol = Symbol <$> lexeme parseIdentifier

parseIntLit :: Parser Term
parseIntLit = IntLit <$> lexeme parseInt

parseApp :: Parser Term
parseApp = lexeme $ do
  symbol "("
  f <- parseTerm
  args <- many parseTerm
  symbol ")"
  pure $  App f args

parseTermSpecAlt :: Parser TermSpecAlt
parseTermSpecAlt = TermSpecAlt <$> lexemeNewline parseTerm

parseVarName :: Parser String
parseVarName = char '?' *> parseIdentifier

parseGrammar :: Parser Grammar
parseGrammar = some parseTermSpec

parseTermSpec :: Parser TermSpec
parseTermSpec = lexemeNewline $ do
  names <- lexemeNewline parseVarName `sepBy1` symbolNewline ","
  symbolNewline "::="
  alts <- parseTermSpecAlt `sepBy1` symbolNewline "|"
  pure (TermSpec names alts)

