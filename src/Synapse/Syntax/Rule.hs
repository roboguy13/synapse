module Synapse.Syntax.Rule where

import Synapse.Syntax.Judgment
import Synapse.Ppr

data Rule =
  Rule
  { ruleName :: Maybe String
  , rulePremises :: [Judgment]
  , ruleConclusion :: Judgment
  }
  deriving (Show)

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

