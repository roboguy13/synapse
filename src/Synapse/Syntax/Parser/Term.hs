module Synapse.Syntax.Parser.Term
  where

import Synapse.Syntax.Term
import Synapse.Syntax.Parser.Utils

import Unbound.Generics.LocallyNameless

import Text.Megaparsec

parseTerm :: Parser Term
parseTerm = undefined

parseVar :: Parser Term
parseVar = Var . string2Name <$> parseIdentifier

parseIntLit :: Parser Term
parseIntLit = IntLit <$> parseInt

parseApp :: Parser Term
parseApp = do
  symbol "("
  f <- parseTerm
  args <- many parseTerm
  symbol ")"
  pure $  App f args

