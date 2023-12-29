{-# LANGUAGE TemplateHaskell #-}

module Synapse.Syntax.Rule where

import Synapse.Syntax.Judgment
import Synapse.Syntax.Context
import Synapse.Syntax.Term
import Synapse.Logic.SubstMap
import Synapse.Logic.Match
import Synapse.Ppr

import Control.Lens hiding ((<.>))

data Rule =
  Rule
  { _ruleName :: Maybe String
  , _rulePremises :: [SomeJudgment]
  , _ruleConclusion :: SomeJudgment
  }
  deriving (Show)

makeLenses ''Rule

instance Simplify Rule where
  simplify rule =
    rule
    { _rulePremises = map simplify $ _rulePremises rule
    , _ruleConclusion = simplify $ _ruleConclusion rule
    }

instance RenameTerms Rule where
  renameTerms xs rule =
    rule
    { _rulePremises = map (renameTerms xs) $ _rulePremises rule
    , _ruleConclusion = renameTerms xs $ _ruleConclusion rule
    }

  termFVs rule =
    concatMap termFVs (_rulePremises rule) ++ termFVs (_ruleConclusion rule)

-- instance HasTerms Rule where
--   terms f (Rule n premises conclusion) =
--     Rule n <$> traverse (terms f) premises <*> terms f conclusion

substMapRule :: SubstMap -> Rule -> Rule
substMapRule substMap rule =
  rule
  { _rulePremises = map (applySubstMap substMap) (_rulePremises rule)
  , _ruleConclusion = applySubstMap substMap (_ruleConclusion rule)
  }

newtype Query = Query SomeJudgment
  deriving Show

instance Ppr Rule where
  ppr rule =
    vcat $
      case _rulePremises rule of
        [] -> [text ""]
        premises -> map ppr premises
        ++
      [text "-------" <+> nameDoc]
        ++
      [ppr (_ruleConclusion rule)]
    where
      nameDoc =
        case _ruleName rule of
          Nothing -> mempty
          Just rName -> text "[" <.> text rName <.> text "]"

instance Ppr Query where
  ppr (Query q) = ppr q

