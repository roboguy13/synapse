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

parseSystem :: Parser System
parseSystem = do
  grammar <- parseGrammar
  jSpecs <- some (parseJudgmentSpec grammar)
  rules <- some (parseRule jSpecs)
  pure $ System grammar jSpecs rules

