--
-- A system of inference rules
--

module Synapse.Syntax.System
  where

import Synapse.Syntax.Term
import Synapse.Syntax.Judgment
import Synapse.Syntax.Rule
import Synapse.Ppr

import Data.List

data System =
  System
  { systemGrammar :: Grammar
  , systemJudgmentSpecs :: [JudgmentSpec]
  , systemRules :: [Rule]
  , systemQueries :: [Query]
  -- , systemSmallStep :: Maybe JudgmentSpec
  -- , systemTyping :: Maybe JudgmentSpec
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
    , nest 2 $ vcat $ intersperse (text "") $ map ppr $ systemRules system
    , text ""
    , text "queries"
    , nest 2 $ vcat $ map ppr $ systemQueries system
    ]
    where
      docMaybe :: Maybe a -> (a -> Doc) -> Doc
      docMaybe Nothing _ = mempty
      docMaybe (Just x) f = f x

