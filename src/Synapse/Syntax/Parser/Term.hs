module Synapse.Syntax.Parser.Term
  where

import Synapse.Syntax.Term
import Synapse.Syntax.Parser.Utils

import Unbound.Generics.LocallyNameless

import Text.Megaparsec
import Text.Megaparsec.Char

parseTerm :: Parser Term
parseTerm = label "term" $
  try parseVar <|> try parseIntLit <|> try parseSymbol <|> try parseApp

parseVar :: Parser Term
parseVar = Var . string2Name <$> lexeme (char '?' *> parseIdentifier)

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

