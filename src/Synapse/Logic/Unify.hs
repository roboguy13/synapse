{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Synapse.Logic.Unify
  (Match (..)
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
import Synapse.Logic.Injection
import Synapse.Logic.ConstrEq

import Unbound.Generics.LocallyNameless

import Control.Monad.Trans
import Control.Applicative
import Control.Lens.Plated

import Data.Typeable
import Data.Foldable

import GHC.Generics

import Data.Maybe

doOccursCheck :: Bool
doOccursCheck = True

data NodePair a = forall b. Match b => NodePair (Injection b a) b b

class (Eq a, Plated a, Subst a a, Typeable a, Alpha a) => Match a where
  isConst :: a -> Bool
  mkVar :: Name a -> a
  isVar :: a -> Maybe (Name a)

  matchConstructor :: a -> a -> Maybe [NodePair a]

  default matchConstructor :: (Generic a, GConstrEq (Rep a), Plated a) => a -> a -> Maybe [NodePair a]
  matchConstructor x y =
    if constrEq x y
    then Just $ zipWith (NodePair id) (children x) (children y)
    else Nothing

  -- isBinder :: a -> Maybe (Int, Name a, a)
  -- isApp :: a -> Maybe (a, a)

  -- mkApp :: a -> a -> a
  -- mkBinder :: Name a -> a -> a

match :: Match a => a -> a -> FreshMT Maybe (Substitution a)
match = matchSubst mempty

unify :: Match a => a -> a -> FreshMT Maybe (Substitution a)
unify = unifySubst mempty

matchList :: Match a => [(a, a)] -> FreshMT Maybe (Substitution a)
matchList = go mempty
  where
    go subst [] = pure subst
    go subst ((x, y) : rest) = do
      subst' <- matchSubst subst x y
      go subst' rest

matchSubst :: Match a => Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
matchSubst = solve matchSolver id

unifySubst :: Match a => Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
unifySubst = solve unifySolver id

type UnifierM = FreshMT Maybe

data Solver a =
  Solver
  { solve :: forall b. Match b => Injection b a -> Substitution a -> b -> b -> UnifierM (Substitution a)
  , solveVarRight :: forall b. Match b => Injection b a -> Substitution a -> Name b -> b -> UnifierM (Substitution a)
  }

unifySolver :: Match a => Solver a
unifySolver = Solver (solveSubstInj unifySolver) (solveVar unifySolver)

matchSolver :: Match a => Solver a
matchSolver = Solver (solveSubstInj matchSolver) (\_ _ _ _ -> lift Nothing)

solveSubstInj :: (Match a, Match b) =>
  Solver a ->
  Injection b a -> Substitution a -> b -> b -> UnifierM (Substitution a)
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
  Injection b a -> Substitution a -> [NodePair b] -> UnifierM (Substitution a)
solveList solver inj sbst [] = pure sbst
solveList solver inj sbst (NodePair injC x y : rest) = do
  sbst' <- solveSubstInj solver (inj . injC) sbst x y
  solveList solver inj sbst' rest

solveVar :: (Match a, Match b) => Solver a -> Injection b a -> Substitution a -> Name b -> b -> UnifierM (Substitution a)
solveVar solver inj sbst v y = do
  guard (not (occurs v y))

  case isVar y of
    Just yV ->
      case lookupInj inj sbst yV of
        Just yInst -> do
          guard (not (occurs v yInst))
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

--
-- --
-- -- Based on the algorithm from "Functional Unification of Higher-Order Patterns" by Tobias Nipkow
-- --
-- data Solver a =
--   Solver
--   { solveFlexFlex :: Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
--   , solveFlexRigid :: Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
--   , solveFlexRigidRight :: Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
--   }
--
-- matchSolver :: Match a => Solver a
-- matchSolver =
--   Solver
--   { solveFlexFlex = flexFlexMatch
--   , solveFlexRigid = flexRigidMatch
--   , solveFlexRigidRight = flexRigidRightMatch
--   }
--
-- unifySolver :: Match a => Solver a
-- unifySolver =
--   Solver
--   { solveFlexFlex = flexFlexUnify
--   , solveFlexRigid = flexRigidUnify
--   , solveFlexRigidRight = flexRigidRightUnify
--   }
--
-- generalUnify :: Match a => Solver a -> Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
-- generalUnify solver sbst s0 t0 =
--   case (devar sbst s0, devar sbst t0) of
--     (s, t)
--       | Just (sortS, x, s) <- isBinder s
--       , Just (sortT, y, t) <- isBinder t
--       , sortS == sortT ->
--           let t' = if x == y then t else subst y (mkVar x) t
--           in
--           generalUnify solver sbst s t'
--
--       | Just (sortS, x, s) <- isBinder s ->
--           generalUnify solver sbst s (mkApp t (mkVar x))
--
--       | Just (sortS, x, t) <- isBinder t ->
--           generalUnify solver sbst (mkApp s (mkVar x)) t
--
--       | otherwise -> nonbinderCases solver sbst s t
-- {-# INLINE generalUnify #-}
--
-- nonbinderCases :: Match a => Solver a -> Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
-- nonbinderCases solver sbst s t =
--   case (strip s, strip t) of
--     ((a, ym), (b, tn))
--       | Just f <- isVar a, isFreeName f
--       , Just g <- isVar b, isFreeName g -> solveFlexFlex solver f ym g tn sbst
--
--       | Just f <- isVar a, isFreeName f -> solveFlexRigid solver f ym t sbst
--
--       | Just f <- isVar b, isFreeName f -> solveFlexRigidRight solver f ym s sbst
--
--       | otherwise -> rigidRigid solver a ym b tn sbst
--
-- flexFlexMatch :: Match a => Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexFlexMatch _ _ _ _ _ = lift Nothing
--
-- flexRigidMatch :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexRigidMatch f ym t sbst =
--   let looseVars = getLooseBoundVars t
--   in
--   if all (`elem` mapMaybe isVar ym) looseVars
--   then pure $ extend sbst f $ abs ym t
--   else lift Nothing
--
-- flexRigidRightMatch :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexRigidRightMatch _ _ _ _ = lift Nothing
--
-- flexFlexUnify :: Match a => Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexFlexUnify f ym g zn sbst =
--   if f == g
--   then flexFlex1 f ym zn sbst
--   else flexFlex2 f ym g zn sbst
--
-- flexRigidRightUnify :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexRigidRightUnify = flexRigidUnify
--
-- flexRigidUnify :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexRigidUnify f ym t sbst =
--   if occursCheck sbst f t
--   then lift Nothing
--   else proj (map unsafeGetVar ym) (extend sbst f (abs ym t)) t
--
-- rigidRigid :: Match a => Solver a -> a -> [a] -> a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
-- rigidRigid solver a ss b ts sbst =
--   if not (a `aeq` b)
--   then lift Nothing
--   else foldlM (uncurry . generalUnify solver) sbst (zip ss ts)
--
-- flexFlex1 :: Match a => Name a -> [a] -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexFlex1 f ym zn sbst = do
--   newV <- fresh (string2Name "alpha")
--   case eqs ym zn of
--     Nothing -> lift Nothing
--     Just eqNames -> pure $ extend sbst f (hnf ym newV eqNames)
--
-- flexFlex2 :: Match a => Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
-- flexFlex2 f ym g zn sbst = do
--   let xk = ym ++ zn
--   h <- fresh (string2Name "alpha")
--   pure $ extend (extend sbst g (hnf zn h xk)) f (hnf ym h xk)
--
-- proj :: Match a => [Name a] -> Substitution a -> a -> FreshMT Maybe (Substitution a)
-- proj w sbst s =
--   case strip (devar sbst s) of
--     (s', ss)
--       | Just (_, x, t) <- isBinder s' -> proj (x : w) sbst t
--       | isConst s' -> foldlM (proj w) sbst ss
--
--       | Just x <- isVar s', isBound x ->
--           if x `elem` w
--           then foldlM (proj w) sbst ss
--           else lift Nothing
--
--       | Just f <- isVar s', isFreeName f -> do
--           newV <- fresh (string2Name "alpha")
--           pure $ extend sbst f (hnf ss newV (ss ++ map mkVar w))
--
-- eqs :: Alpha a => [a] -> [a] -> Maybe [a]
-- eqs (x:xs) (y:ys) =
--   if x `aeq` y
--   then (x :) <$> eqs xs ys
--   else eqs xs ys
-- eqs [] [] = Just []
-- eqs _ _ = Nothing
--
-- strip :: Match a => a -> (a, [a])
-- strip t0 = go (t0, [])
--   where
--     go (t0, acc)
--       | Just (s, t) <- isApp t0 = go (s, t : acc)
--     go p = p
--
-- red :: Match a => a -> [a] -> a
-- red s0 (y : ys)
--   | Just (_, x, s) <- isBinder s0 = red (subst x y s) ys
-- red s (y : ys)                    = red (mkApp s y) ys
-- red s []                          = s
--
-- devar :: Match a => Substitution a -> a -> a
-- devar sbst t =
--   case strip t of
--     (a, ys)
--       | Just x <- isVar a ->
--           case Substitution.lookup x sbst of
--             Just s -> devar sbst (red t ys)
--       | otherwise -> t
--
-- occursCheck :: Match a => 
--   Substitution a -> Name a -> a -> Bool
-- occursCheck =
--   if doOccursCheck
--   then \_ _ _ -> False
--   else go
--   where
--     go sbst v t
--       | Just xT <- isVar t =
--           xT `aeq` v
--             || case lookup xT sbst of
--                  Just s -> go sbst v s
--                  Nothing -> False
--       | otherwise          = any (go sbst v) $ getChildren t
--
-- unsafeGetVar :: Match a => a -> Name a
-- unsafeGetVar (isVar -> Just x) = x
--
-- abs :: Match a => [a] -> a -> a
-- abs xs t = foldr (mkBinder . unsafeGetVar) t xs
--
-- hnf :: Match a => [a] -> Name a -> [a] -> a
-- hnf xs f ss = abs xs (foldl mkApp (mkVar f) ss)
--
-- isBound :: Name a -> Bool
-- isBound = not . isFreeName
--
-- getLooseBoundVars :: Match a => a -> [Name a]
-- getLooseBoundVars = go []
--   where
--     go inScopeNames t
--       | Just x <- isVar t =
--           if isBound x && x `notElem` inScopeNames
--           then [x]
--           else []
--       | Just (_, x, body) <- isBinder t = go (x : inScopeNames) body
--       | ts <- getChildren t = concatMap (go inScopeNames) ts
--

