--
-- A system of inference rules
--

module Synapse.Syntax.System
  where

import Synapse.Syntax.Term
import Synapse.Syntax.Judgment
import Synapse.Syntax.Rule

data System =
  System
  { systemTermSpecs :: [TermSpec]
  , systemJudgmentSpecs :: [JudgmentSpec]
  , systemRules :: [Rule]
  }

