{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
  ,matchM
  ,unifyM
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
import Control.Lens

import Data.Typeable
import Data.Foldable

import Data.Maybe

import Data.Dependent.Map
import Data.Proxy

doOccursCheck :: Bool
doOccursCheck = True

match :: Match a => a -> a -> Maybe SubstMap
match x = runFreshMT . matchM x

unify :: Match a => a -> a -> Maybe SubstMap
unify x = runFreshMT . unifyM x

matchM :: Match a => a -> a -> FreshMT Maybe SubstMap
matchM = matchSubst mempty

unifyM :: Match a => a -> a -> FreshMT Maybe SubstMap
unifyM = unifySubst mempty

matchList :: Match a => [(a, a)] -> FreshMT Maybe SubstMap
matchList = go mempty
  where
    go subst [] = pure subst
    go subst ((x, y) : rest) = do
      subst' <- matchSubst subst x y
      go subst' rest

matchSubst :: Match a => SubstMap -> a -> a -> FreshMT Maybe SubstMap
matchSubst = solve matchSolver

unifySubst :: Match a => SubstMap -> a -> a -> FreshMT Maybe SubstMap
unifySubst = solve unifySolver

type UnifierM = FreshMT Maybe

data Solver =
  Solver
  { solve :: forall a. Match a => SubstMap -> a -> a -> UnifierM SubstMap
  , solveVarRight :: forall a. Match a => SubstMap -> Name a -> a -> UnifierM SubstMap
  }

unifySolver :: Solver
unifySolver = Solver (solveSubstMap unifySolver) (solveVar unifySolver)

matchSolver :: Solver
matchSolver = Solver (solveSubstMap matchSolver) (\_ _ _ -> lift Nothing)

solveSubstMap :: Match a =>
  Solver ->
  SubstMap -> a -> a -> UnifierM SubstMap
solveSubstMap solver sbst x y
  | isConst x, isConst y = do
      guard (x == y)
      pure sbst

  | Just xV <- isVar x = solveVar solver sbst xV y
  | Just yV <- isVar y = solveVarRight solver sbst yV x

  | otherwise = do
      ps <- lift (matchConstructor x y)
      solveList solver sbst ps

solveList :: forall a. Match a =>
  Solver ->
  SubstMap -> [NodePair a] -> UnifierM SubstMap
solveList solver sbst [] = pure sbst
solveList solver sbst (NodePair x y : rest) = do
  sbst' <- solveSubstMap solver sbst x y
  solveList solver sbst' rest

solveVar :: Match a => Solver -> SubstMap -> Name a -> a -> UnifierM SubstMap
solveVar solver sbst v y = do
  guard (not (occurs v y))

  case isVar y of
    Just yV ->
      case sbst ^. (substLens . to (Substitution.lookup yV)) of 
        Just yInst -> do
          guard (not (occurs v yInst))
          mkVar <- lift mkVar_maybe
          solveSubstMap solver sbst (mkVar v) yInst

        Nothing ->
          pure $ sbst & substLens %~ extend v y

    Nothing ->
      pure $ sbst & substLens %~ extend v y

occurs :: Match a => Name a -> a -> Bool
occurs =
  if not doOccursCheck
  then \_ _ -> False
  else \v x ->
    case isVar x of
      Just xV -> xV `aeq` v -- TODO: Is this right?
      Nothing -> any (occurs v) (children x)

