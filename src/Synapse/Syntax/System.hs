--
-- A system of inference rules
--

module Synapse.Syntax.System
  where

import Synapse.Syntax.Term
import Synapse.Syntax.Judgment
import Synapse.Syntax.Rule
import Synapse.Ppr

data System =
  System
  { systemGrammar :: Grammar
  , systemJudgmentSpecs :: [JudgmentSpec]
  , systemRules :: [Rule]
  }

instance Ppr System where
  ppr system =
    vcat
    [ ppr $ systemGrammar system
    , vcat $ map ppr $ systemJudgmentSpecs system
    , vcat $ map ppr $ systemRules system
    ]

