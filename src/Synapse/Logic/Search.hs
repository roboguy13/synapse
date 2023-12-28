{-# LANGUAGE RankNTypes #-}

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
import Synapse.Ppr

import Data.Bifunctor
import Data.Coerce

import Unbound.Generics.LocallyNameless

import Control.Lens
import Control.Monad.Trans

import Data.Maybe

import Debug.Trace

data QueryResult =
  QueryResult
  { queryResultDerivation :: Derivation SomeJudgment
  , queryResultTermSubst :: Substitution SubstTerm
  -- , queryResultJudgmentSubst :: Substitution SomeJudgment
  }
  deriving Show

instance Ppr QueryResult where
  ppr = ppr . queryResultDerivation

runQuery :: [Rule] -> Query -> [QueryResult]
runQuery rules0 =
  let rules = runFreshM $ traverse freshenRule rules0
  in
  -- traceShow rules $
  runQueryFreshened rules substMapEmpty

runQueryFreshened :: [Rule] -> SubstMap -> Query -> [QueryResult]
runQueryFreshened rules substMap (Query query) = do
  (matchingRule, substMap') <- mapMaybe (matchRule substMap query) rules

  case _rulePremises matchingRule of
    [] -> pure $ QueryResult (derivationOne matchingRule) (substMap' ^. substLens)
    subgoals -> do
      subResult <- traverse (runQueryFreshened rules substMap' . Query) subgoals
      let subDerivation = map queryResultDerivation subResult
      pure $ QueryResult (DerivationStep (_ruleName matchingRule) (_ruleConclusion matchingRule) subDerivation) (substMap' ^. substLens)

matchRule :: SubstMap -> SomeJudgment -> Rule -> Maybe (Rule, SubstMap)
matchRule substMap y rule = do
  substMap' <- runFreshMT $ matchSubst substMap (_ruleConclusion rule) y
  trace ("substMap = " ++ show (substMap' ^. substLens :: Substitution SubstTerm)) $ pure (substMapRule substMap' rule, substMap')

freshenRule :: Rule -> FreshM Rule
freshenRule rule = do
  let theFVs :: [Name Term]
      theFVs = coerce $ termFVs rule
  renaming <- traverse freshenName theFVs
  pure $ renameTerms renaming rule

freshenName :: Name a -> FreshM (Name a, Name a)
freshenName x = do
  y <- fresh x
  pure (x, y)

derivationOne :: Rule -> Derivation SomeJudgment
derivationOne rule = DerivationStep (_ruleName rule) (_ruleConclusion rule) []

