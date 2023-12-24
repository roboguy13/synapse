module Synapse.Logic.Search
  where

import Synapse.Logic.Derivation
import Synapse.Logic.SubstMap
import Synapse.Syntax.Rule
import Synapse.Syntax.Context
import Synapse.Syntax.Term
import Synapse.Syntax.Judgment
import Synapse.Logic.Substitution
import Synapse.Logic.Unify

import Control.Lens

import Data.Maybe

data QueryResult =
  QueryResult
  { queryResultDerivation :: Derivation SomeJudgment
  , queryResultTermSubst :: Substitution Term
  -- , queryResultJudgmentSubst :: Substitution SomeJudgment
  }

runQuery :: [Rule] -> Query -> [QueryResult]
runQuery rules (Query query) = do
  (matchingRule, substMap) <- mapMaybe (`matchRule` query) rules

  case rulePremises matchingRule of
    [] -> pure $ QueryResult (derivationOne matchingRule) (substMap ^. substLens)
    subgoals -> do
      subgoal <- subgoals
      let subResult = runQuery rules $ Query subgoal
          subDerivation = queryResultDerivation <$> subResult
      pure $ QueryResult (DerivationStep (ruleConclusion matchingRule) subDerivation) (substMap ^. substLens)

matchRule :: Rule -> SomeJudgment -> Maybe (Rule, SubstMap)
matchRule x y = do
  substMap <- match y (ruleConclusion x)
  pure (substMapRule substMap x, substMap)

derivationOne :: Rule -> Derivation SomeJudgment
derivationOne rule = DerivationStep (ruleConclusion rule) []
