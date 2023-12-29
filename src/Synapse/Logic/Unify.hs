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
{-# LANGUAGE LambdaCase #-}

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

import Control.Monad.Trans.Maybe

import Synapse.Logic.Substitution as Substitution
import Synapse.Logic.SubstMap
import Synapse.Logic.Propagator
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
  runMaybeT (matchCells x y) >>= \case
    Nothing -> pure Nothing
    Just substMap -> do
      runMaybeT (runFreshMT (convertSubstMap substMap)) >>= \case
        Nothing -> pure Nothing
        Just substMap' -> pure $ Just substMap'

unify :: Match a => a -> a -> Maybe SubstMap
unify x y = runST $ do
  runMaybeT (unifyCells x y) >>= \case
    Nothing -> pure Nothing
    Just substMap -> do
      runMaybeT (runFreshMT (convertSubstMap substMap)) >>= \case
        Nothing -> pure Nothing
        Just substMap' -> pure $ Just substMap'

matchCells :: Match a => a -> a -> MaybeT (ST s) (USubstMapCells s)
matchCells x = runFreshMT . matchM x

unifyCells :: Match a => a -> a -> MaybeT (ST s) (USubstMapCells s)
unifyCells x = runFreshMT . unifyM x

matchM :: Match a => a -> a -> FreshMT (MaybeT (ST s)) (USubstMapCells s)
matchM = matchSubst hlistEmpty

unifyM :: Match a => a -> a -> FreshMT (MaybeT (ST s)) (USubstMapCells s)
unifyM = unifySubst hlistEmpty

matchList :: Match a => [(a, a)] -> FreshMT (MaybeT (ST s)) (USubstMapCells s)
matchList = go hlistEmpty
  where
    go subst [] = pure subst
    go subst ((x, y) : rest) = do
      subst' <- matchSubst subst x y
      go subst' rest

matchSubst :: Match a => USubstMapCells s -> a -> a -> FreshMT (MaybeT (ST s)) (USubstMapCells s)
matchSubst = solve matchSolver

unifySubst :: Match a => USubstMapCells s -> a -> a -> FreshMT (MaybeT (ST s)) (USubstMapCells s)
unifySubst = solve unifySolver

type UnifierM s = FreshMT (MaybeT (ST s))

type UCell s = Cell (UnifierM s)
type USubstMapCells s = SubstMapCells (UnifierM s)

data Solver s =
  Solver
  { solve :: forall a. Match a => USubstMapCells s -> a -> a -> UnifierM s (USubstMapCells s)
  , solveVarRight :: forall a. Match a => USubstMapCells s -> Name a -> a -> UnifierM s (USubstMapCells s)
  }

unifySolver :: Solver s
unifySolver = Solver (solveSubstMap unifySolver) (solveVar unifySolver)

matchSolver :: Solver s
matchSolver = Solver (solveSubstMap matchSolver) (\_ _ _ -> lift $ hoistMaybe Nothing)

solveSubstMap :: Match a =>
  Solver s ->
  USubstMapCells s -> a -> a -> UnifierM s (USubstMapCells s)
solveSubstMap solver sbst x y
  | isConst x, isConst y = do
      guard (x == y)
      pure sbst

  | Just xV <- isVar x = solveVar solver sbst xV y
  | Just yV <- isVar y = solveVarRight solver sbst yV x

  | otherwise = do
      let y' = simplify y
      ps <- lift $ hoistMaybe (matchConstructor x y')
      solveList solver sbst ps

solveList :: forall s a. Match a =>
  Solver s ->
  USubstMapCells s -> [NodePair a] -> UnifierM s (USubstMapCells s)
solveList solver sbst [] = pure sbst
solveList solver sbst (NodePair x y : rest) = do
  sbst' <- solveSubstMap solver sbst x y
  solveList solver sbst' rest

-- | Create the cell if it doesn't exist
getVarCell :: Typeable a => USubstMapCells s -> Name a -> UnifierM s (UCell s a, USubstMapCells s)
getVarCell sbst v =
  case sbst ^. (substLens . to (lookupSubstCell v)) of
    Just cell -> pure (cell, sbst)
    Nothing -> do
      (cell, sbstCells) <- mkUnknownSubstCell (sbst ^. substLens) v
      pure (cell, sbst & substLens .~ sbstCells)

solveVar :: forall s a. Match a => Solver s -> USubstMapCells s -> Name a -> a -> UnifierM s (USubstMapCells s)
solveVar solver sbst0 xVar y = do
  guard (not (occurs xVar y))

  (cellX, sbst') <- getVarCell sbst0 xVar

  case isVar y of
    Just yVar -> do
      (cellY, sbst'') <- getVarCell sbst' yVar
      mirror cellX cellY
      binaryRel (fmap void . solveSubstMap solver sbst'') cellX cellY
      pure sbst''

      -- solveSubstMap solver sbst'' (mkVar xVar) undefined

    Nothing -> do
      -- TODO: Do we also unify here?
      sb <- writeSubstCell (sbst' ^. substLens) xVar y
      pure $ sbst' & substLens .~ sb

      -- case sbst ^. (substLens . to (lookupSubstCell yVar)) of
      --   Just yCell -> undefined
      --
      --   Nothing -> do
      --     (yCell, newSbst) <- lift $ lift $ mkUnknownSubstCell (sbst ^. substLens) yVar
      --     undefined

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

occurs :: Match a => Name a -> a -> Bool
occurs =
  if not doOccursCheck
  then \_ _ -> False
  else \v x ->
    case isVar x of
      Just xV -> xV `aeq` v -- TODO: Is this right?
      Nothing -> any (occurs v) (children x)

