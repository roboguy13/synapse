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
    [ text "grammar"
    , nest 2 $ ppr $ systemGrammar system
    , text ""
    , text "judgments"
    , nest 2 $ vcat $ map ppr $ systemJudgmentSpecs system
    , text ""
    , text "rules"
    , nest 2 $ vcat $ map ppr $ systemRules system
    ]

