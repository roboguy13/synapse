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
  ,module Synapse.Logic.Match
  )
  where

import Prelude hiding (abs, lookup, id, (.))

import Control.Category
import Control.Monad

import Synapse.Logic.Substitution as Substitution
import Synapse.Logic.SubstMap
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

match :: (SubstMapSing (ContainedTypes a), Match a) => a -> a -> FreshMT Maybe (MatchSubst a)
match = matchSubst mempty 

unify :: (SubstMapSing (ContainedTypes a), Match a) => a -> a -> FreshMT Maybe (MatchSubst a)
unify = unifySubst mempty

matchList :: (SubstMapSing (ContainedTypes a), Match a) => [(a, a)] -> FreshMT Maybe (MatchSubst a)
matchList = go mempty
  where
    go subst [] = pure subst
    go subst ((x, y) : rest) = do
      subst' <- matchSubst subst x y
      go subst' rest

matchSubst :: Match a => MatchSubst a -> a -> a -> FreshMT Maybe (MatchSubst a)
matchSubst = solve matchSolver idPath

unifySubst :: Match a => MatchSubst a -> a -> a -> FreshMT Maybe (MatchSubst a)
unifySubst = solve unifySolver idPath

type UnifierM = FreshMT Maybe

data Solver a =
  Solver
  { solve :: forall b. Match b => Path a b -> MatchSubst a -> b -> b -> UnifierM (MatchSubst a)
  , solveVarRight :: forall b. Match b => Path a b -> MatchSubst a -> Name b -> b -> UnifierM (MatchSubst a)
  }

unifySolver :: Match a => Solver a
unifySolver = Solver (solveSubstInj unifySolver) (solveVar unifySolver)

matchSolver :: Match a => Solver a
matchSolver = Solver (solveSubstInj matchSolver) (\_ _ _ _ -> lift Nothing)

solveSubstInj :: (Match a, Match b) =>
  Solver a ->
  Path a b -> MatchSubst a -> b -> b -> UnifierM (MatchSubst a)
solveSubstInj solver path sbst x y
  | isConst x, isConst y = do
      guard (x == y)
      pure sbst

  | Just xV <- isVar x = solveVar solver path sbst xV y
  | Just yV <- isVar y = solveVarRight solver path sbst yV x

  | otherwise = do
      ps <- lift (matchConstructor x y)
      solveList solver path sbst ps

solveList :: forall a b. (Match a, Match b) =>
  Solver a ->
  Path a b -> MatchSubst a -> [NodePair b] -> UnifierM (MatchSubst a)
solveList solver path sbst [] = pure sbst
solveList solver path sbst (NodePair pathC x y : rest) = do
  sbst' <- solveSubstInj solver (pathC . path) sbst x y
  solveList solver path sbst' rest

solveVar :: (Match a, Match b) => Solver a -> Path a b -> MatchSubst a -> Name b -> b -> UnifierM (MatchSubst a)
solveVar solver path sbst v y = do
  guard (not (occurs v y))

  case isVar y of
    Just yV ->
      case pathLookup path sbst yV of
        Just yInst -> do
          guard (not (occurs v yInst))
          mkVar <- lift mkVar_maybe
          solveSubstInj solver path sbst (mkVar v) yInst

        Nothing ->
          pure $ pathExtend path sbst v y

    Nothing ->
      pure $ pathExtend path sbst v y

occurs :: Match a => Name a -> a -> Bool
occurs =
  if not doOccursCheck
  then \_ _ -> False
  else \v x ->
    case isVar x of
      Just xV -> xV `aeq` v -- TODO: Is this right?
      Nothing -> any (occurs v) (children x)

