module Synapse.Syntax.Parser.System
  where

import Synapse.Syntax.System
import Synapse.Syntax.Term
import Synapse.Syntax.Judgment
import Synapse.Syntax.Rule
import Synapse.Syntax.Parser.Term
import Synapse.Syntax.Parser.Judgment
import Synapse.Syntax.Parser.Rule
import Synapse.Syntax.Parser.Utils

import Text.Megaparsec
import Text.Megaparsec.Char

parseSystem :: Parser System
parseSystem = do
  keywordNewline "grammar"
  grammar <- parseGrammar
  keywordNewline "judgments"
  jSpecs <- some (parseJudgmentSpec grammar)
  keywordNewline "rules"
  rules <- some (parseRule jSpecs)

  queriesMaybe <- optional $ do
    keywordNewline "queries"
    some (parseQuery jSpecs)

  pure $ System grammar jSpecs rules (concat queriesMaybe)

