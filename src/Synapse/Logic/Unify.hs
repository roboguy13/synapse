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

import Control.Monad.ST

import Debug.Trace

doOccursCheck :: Bool
doOccursCheck = True

match :: Match a => a -> a -> Maybe SubstMap
match x y = runST $
  case matchCells x y of
    Nothing -> pure Nothing
    Just substMap -> do
      sm <- convertSubstMap substMap
      pure $ Just sm

unify :: Match a => a -> a -> Maybe SubstMap
unify x y = runST $
  case unifyCells x y of
    Nothing -> pure Nothing
    Just substMap -> do
      sm <- convertSubstMap substMap
      pure $ Just sm

matchCells :: Match a => a -> a -> Maybe (SubstMapCells s)
matchCells x = runFreshMT . matchM x

unifyCells :: Match a => a -> a -> Maybe (SubstMapCells s)
unifyCells x = runFreshMT . unifyM x

matchM :: Match a => a -> a -> FreshMT Maybe (SubstMapCells s)
matchM = matchSubst hlistEmpty

unifyM :: Match a => a -> a -> FreshMT Maybe (SubstMapCells s)
unifyM = unifySubst hlistEmpty

matchList :: Match a => [(a, a)] -> FreshMT Maybe (SubstMapCells s)
matchList = go hlistEmpty
  where
    go subst [] = pure subst
    go subst ((x, y) : rest) = do
      subst' <- matchSubst subst x y
      go subst' rest

matchSubst :: Match a => SubstMapCells s -> a -> a -> FreshMT Maybe (SubstMapCells s)
matchSubst = solve matchSolver

unifySubst :: Match a => SubstMapCells s -> a -> a -> FreshMT Maybe (SubstMapCells s)
unifySubst = solve unifySolver

type UnifierM = FreshMT Maybe

data Solver s =
  Solver
  { solve :: forall a. Match a => SubstMapCells s -> a -> a -> UnifierM (SubstMapCells s)
  , solveVarRight :: forall a. Match a => SubstMapCells s -> Name a -> a -> UnifierM (SubstMapCells s)
  }

unifySolver :: Solver s
unifySolver = Solver (solveSubstMap unifySolver) (solveVar unifySolver)

matchSolver :: Solver s
matchSolver = Solver (solveSubstMap matchSolver) (\_ _ _ -> lift Nothing)

solveSubstMap :: Match a =>
  Solver s ->
  SubstMapCells s -> a -> a -> UnifierM (SubstMapCells s)
solveSubstMap solver sbst x y
  | isConst x, isConst y = do
      guard (x == y)
      pure sbst

  | Just xV <- isVar x = solveVar solver sbst xV y
  | Just yV <- isVar y = solveVarRight solver sbst yV x

  | otherwise = do
      let y' = simplify y
      ps <- lift (matchConstructor x y')
      solveList solver sbst ps

solveList :: forall s a. Match a =>
  Solver s ->
  SubstMapCells s -> [NodePair a] -> UnifierM (SubstMapCells s)
solveList solver sbst [] = pure sbst
solveList solver sbst (NodePair x y : rest) = do
  sbst' <- solveSubstMap solver sbst x y
  solveList solver sbst' rest

solveVar :: forall s a. Match a => Solver s -> SubstMapCells s -> Name a -> a -> UnifierM (SubstMapCells s)
solveVar solver sbst v y0 = do
    undefined
--   let y = simplify y0
--   guard (not (occurs v y))
--
--   case isVar y of
--     Just yV ->
--       case sbst ^. (substLens . to (Substitution.lookup yV)) of 
--         Just yInst -> do
--           guard (not (occurs v yInst))
--           mkVar <- lift mkVar_maybe
--           solveSubstMap solver sbst (mkVar v) yInst
--
--         Nothing ->
--           pure $ sbst & substLens %~ extend v y
--
--     Nothing ->
--       case sbst ^. (substLens . to (Substitution.lookup v)) of
--         Just vInst -> do
--           guard (vInst == y)
--           pure sbst
--         Nothing -> pure $ sbst & substLens %~ extend v y
--
-- occurs :: Match a => Name a -> a -> Bool
-- occurs =
--   if not doOccursCheck
--   then \_ _ -> False
--   else \v x ->
--     case isVar x of
--       Just xV -> xV `aeq` v -- TODO: Is this right?
--       Nothing -> any (occurs v) (children x)
--
