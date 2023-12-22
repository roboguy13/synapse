{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Synapse.Logic.Unify
  (NodePair (..)
  ,Match (..)
  ,match
  ,unify
  ,matchList
  ,matchSubst
  ,unifySubst

  ,Substitution
  ,applySubstitution
  )
  where

import Prelude hiding (abs, lookup, id, (.))

import Control.Category
import Control.Monad

import Synapse.Logic.Substitution as Substitution
import Synapse.Logic.SubstMap
import Synapse.Logic.Injection
import Synapse.Logic.ConstrEq
import Synapse.Logic.Match

import Unbound.Generics.LocallyNameless

import Control.Monad.Trans
import Control.Monad.Writer
import Control.Applicative
import Control.Lens.Plated

import Data.Typeable
import Data.Foldable

import GHC.Generics

import Data.Maybe

import Data.Dependent.Map
import Data.Proxy

doOccursCheck :: Bool
doOccursCheck = True

match :: Match a => a -> a -> FreshMT Maybe (MatchSubst a)
match = matchSubst mempty 

unify :: Match a => a -> a -> FreshMT Maybe (MatchSubst a)
unify = unifySubst mempty

matchList :: Match a => [(a, a)] -> FreshMT Maybe (MatchSubst a)
matchList = go mempty
  where
    go subst [] = pure subst
    go subst ((x, y) : rest) = do
      subst' <- matchSubst subst x y
      go subst' rest

matchSubst :: Match a => MatchSubst a -> a -> a -> FreshMT Maybe (MatchSubst a)
matchSubst = solve matchSolver id

unifySubst :: Match a => MatchSubst a -> a -> a -> FreshMT Maybe (MatchSubst a)
unifySubst = solve unifySolver id

type UnifierM = FreshMT Maybe

data Solver a =
  Solver
  { solve :: forall b. Match b => Injection b a -> MatchSubst a -> b -> b -> UnifierM (MatchSubst a)
  , solveVarRight :: forall b. Match b => Injection b a -> MatchSubst a -> Name b -> b -> UnifierM (MatchSubst a)
  }

unifySolver :: Match a => Solver a
unifySolver = Solver (solveSubstInj unifySolver) (solveVar unifySolver)

matchSolver :: Match a => Solver a
matchSolver = Solver (solveSubstInj matchSolver) (\_ _ _ _ -> lift Nothing)

solveSubstInj :: (Match a, Match b) =>
  Solver a ->
  Injection b a -> MatchSubst a -> b -> b -> UnifierM (MatchSubst a)
solveSubstInj solver inj sbst x y
  | isConst x, isConst y = do
      guard (x == y)
      pure sbst

  | Just xV <- isVar x = solveVar solver inj sbst xV y
  | Just yV <- isVar y = solveVarRight solver inj sbst yV x

  | otherwise = do
      ps <- lift (matchConstructor x y)
      solveList solver inj sbst ps

solveList :: forall a b. (Match a, Match b) =>
  Solver a ->
  Injection b a -> MatchSubst a -> [NodePair b] -> UnifierM (MatchSubst a)
solveList solver inj sbst [] = pure sbst
solveList solver inj sbst (InjPair injC x y : rest) = do
  sbst' <- solveSubstInj solver (inj . injC) sbst x y
  solveList solver inj sbst' rest

solveVar :: (Match a, Match b) => Solver a -> Injection b a -> MatchSubst a -> Name b -> b -> UnifierM (MatchSubst a)
solveVar solver inj sbst v y = do
  guard (not (occurs v y))

  case isVar y of
    Just yV ->
      case lookupInj inj sbst yV of
        Just yInst -> do
          guard (not (occurs v yInst))
          mkVar <- lift mkVar_maybe
          solveSubstInj solver inj sbst (mkVar v) yInst

        Nothing ->
          pure $ extendInj inj sbst v y

    Nothing ->
      pure $ extendInj inj sbst v y

occurs :: Match a => Name a -> a -> Bool
occurs =
  if not doOccursCheck
  then \_ _ -> False
  else \v x ->
    case isVar x of
      Just xV -> xV `aeq` v -- TODO: Is this right?
      Nothing -> any (occurs v) (children x)

