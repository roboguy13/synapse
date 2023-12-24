module Synapse.Syntax.Rule where

import Synapse.Syntax.Judgment
import Synapse.Syntax.Context
import Synapse.Logic.SubstMap
import Synapse.Logic.Match
import Synapse.Ppr

data Rule =
  Rule
  { ruleName :: Maybe String
  , rulePremises :: [SomeJudgment]
  , ruleConclusion :: SomeJudgment
  }
  deriving (Show)

substMapRule :: SubstMap -> Rule -> Rule
substMapRule substMap rule =
  rule
  { rulePremises = map (applySubstMap substMap) (rulePremises rule)
  , ruleConclusion = applySubstMap substMap (ruleConclusion rule)
  }

newtype Query = Query SomeJudgment
  deriving Show

instance Ppr Rule where
  ppr rule =
    vcat $
      case rulePremises rule of
        [] -> [text ""]
        premises -> map ppr premises
        ++
      [text "-------" <+> nameDoc]
        ++
      [ppr (ruleConclusion rule)]
    where
      nameDoc =
        case ruleName rule of
          Nothing -> mempty
          Just rName -> text "[" <.> text rName <.> text "]"

instance Ppr Query where
  ppr (Query q) = ppr q

