--
-- Based on the algorithm from "Functional Unification of Higher-Order Patterns" by Tobias Nipkow
--

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Synapse.Logic.Unify
  (Match (..)
  ,match
  ,unify
  ,matchSubst
  ,unifySubst

  ,Substitution
  ,applySubstitution
  )
  where

import Prelude hiding (abs, lookup)

import Synapse.Logic.Substitution as Substitution

import Unbound.Generics.LocallyNameless

import Control.Monad.Trans

import Data.Typeable
import Data.Foldable

import Data.Maybe

doOccursCheck :: Bool
doOccursCheck = True

class (Subst a a, Typeable a, Alpha a) => Match a where
  isConst :: a -> Bool
  mkVar :: Name a -> a
  isVar :: a -> Maybe (Name a)

  -- matchConstructor :: a -> a -> Maybe [(a, a)]
  getChildren :: a -> [a]

  isBinder :: a -> Maybe (Int, Name a, a)
  isApp :: a -> Maybe (a, a)

  mkApp :: a -> a -> a
  mkBinder :: Name a -> a -> a

match :: Match a => a -> a -> FreshMT Maybe (Substitution a)
match = matchSubst mempty

unify :: Match a => a -> a -> FreshMT Maybe (Substitution a)
unify = unifySubst mempty

matchSubst :: Match a => Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
matchSubst = generalUnify matchSolver

unifySubst :: Match a => Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
unifySubst = generalUnify unifySolver

data Solver a =
  Solver
  { solveFlexFlex :: Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
  , solveFlexRigid :: Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
  , solveFlexRigidRight :: Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
  }

matchSolver :: Match a => Solver a
matchSolver =
  Solver
  { solveFlexFlex = flexFlexMatch
  , solveFlexRigid = flexRigidMatch
  , solveFlexRigidRight = flexRigidRightMatch
  }

unifySolver :: Match a => Solver a
unifySolver =
  Solver
  { solveFlexFlex = flexFlexUnify
  , solveFlexRigid = flexRigidUnify
  , solveFlexRigidRight = flexRigidRightUnify
  }

generalUnify :: Match a => Solver a -> Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
generalUnify solver sbst s0 t0 =
  case (devar sbst s0, devar sbst t0) of
    (s, t)
      | Just (sortS, x, s) <- isBinder s
      , Just (sortT, y, t) <- isBinder t
      , sortS == sortT ->
          let t' = if x == y then t else subst y (mkVar x) t
          in
          generalUnify solver sbst s t'

      | Just (sortS, x, s) <- isBinder s ->
          generalUnify solver sbst s (mkApp t (mkVar x))

      | Just (sortS, x, t) <- isBinder t ->
          generalUnify solver sbst (mkApp s (mkVar x)) t

      | otherwise -> nonbinderCases solver sbst s t
{-# INLINE generalUnify #-}

nonbinderCases :: Match a => Solver a -> Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
nonbinderCases solver sbst s t =
  case (strip s, strip t) of
    ((a, ym), (b, tn))
      | Just f <- isVar a, isFreeName f
      , Just g <- isVar b, isFreeName g -> solveFlexFlex solver f ym g tn sbst

      | Just f <- isVar a, isFreeName f -> solveFlexRigid solver f ym t sbst

      | Just f <- isVar b, isFreeName f -> solveFlexRigidRight solver f ym s sbst

      | otherwise -> rigidRigid solver a ym b tn sbst

flexFlexMatch :: Match a => Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
flexFlexMatch _ _ _ _ _ = lift Nothing

flexRigidMatch :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
flexRigidMatch f ym t sbst =
  let looseVars = getLooseBoundVars t
  in
  if all (`elem` mapMaybe isVar ym) looseVars
  then pure $ extend sbst f $ abs ym t
  else lift Nothing

flexRigidRightMatch :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
flexRigidRightMatch _ _ _ _ = lift Nothing

flexFlexUnify :: Match a => Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
flexFlexUnify f ym g zn sbst =
  if f == g
  then flexFlex1 f ym zn sbst
  else flexFlex2 f ym g zn sbst

flexRigidRightUnify :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
flexRigidRightUnify = flexRigidUnify

flexRigidUnify :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
flexRigidUnify f ym t sbst =
  if occursCheck sbst f t
  then lift Nothing
  else proj (map unsafeGetVar ym) (extend sbst f (abs ym t)) t

rigidRigid :: Match a => Solver a -> a -> [a] -> a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
rigidRigid solver a ss b ts sbst =
  if not (a `aeq` b)
  then lift Nothing
  else foldlM (uncurry . generalUnify solver) sbst (zip ss ts)

flexFlex1 :: Match a => Name a -> [a] -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
flexFlex1 f ym zn sbst = do
  newV <- fresh (string2Name "alpha")
  case eqs ym zn of
    Nothing -> lift Nothing
    Just eqNames -> pure $ extend sbst f (hnf ym newV eqNames)

flexFlex2 :: Match a => Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
flexFlex2 f ym g zn sbst = do
  let xk = ym ++ zn
  h <- fresh (string2Name "alpha")
  pure $ extend (extend sbst g (hnf zn h xk)) f (hnf ym h xk)

proj :: Match a => [Name a] -> Substitution a -> a -> FreshMT Maybe (Substitution a)
proj w sbst s =
  case strip (devar sbst s) of
    (s', ss)
      | Just (_, x, t) <- isBinder s' -> proj (x : w) sbst t
      | isConst s' -> foldlM (proj w) sbst ss

      | Just x <- isVar s', isBound x ->
          if x `elem` w
          then foldlM (proj w) sbst ss
          else lift Nothing

      | Just f <- isVar s', isFreeName f -> do
          newV <- fresh (string2Name "alpha")
          pure $ extend sbst f (hnf ss newV (ss ++ map mkVar w))

eqs :: Alpha a => [a] -> [a] -> Maybe [a]
eqs (x:xs) (y:ys) =
  if x `aeq` y
  then (x :) <$> eqs xs ys
  else eqs xs ys
eqs [] [] = Just []
eqs _ _ = Nothing

strip :: Match a => a -> (a, [a])
strip t0 = go (t0, [])
  where
    go (t0, acc)
      | Just (s, t) <- isApp t0 = go (s, t : acc)
    go p = p

red :: Match a => a -> [a] -> a
red s0 (y : ys)
  | Just (_, x, s) <- isBinder s0 = red (subst x y s) ys
red s (y : ys)                    = red (mkApp s y) ys
red s []                          = s

devar :: Match a => Substitution a -> a -> a
devar sbst t =
  case strip t of
    (a, ys)
      | Just x <- isVar a ->
          case Substitution.lookup x sbst of
            Just s -> devar sbst (red t ys)
      | otherwise -> t

occursCheck :: Match a => 
  Substitution a -> Name a -> a -> Bool
occursCheck =
  if doOccursCheck
  then \_ _ _ -> False
  else go
  where
    go sbst v t
      | Just xT <- isVar t =
          xT `aeq` v
            || case lookup xT sbst of
                 Just s -> go sbst v s
                 Nothing -> False
      | otherwise          = any (go sbst v) $ getChildren t

unsafeGetVar :: Match a => a -> Name a
unsafeGetVar (isVar -> Just x) = x

abs :: Match a => [a] -> a -> a
abs xs t = foldr (mkBinder . unsafeGetVar) t xs

hnf :: Match a => [a] -> Name a -> [a] -> a
hnf xs f ss = abs xs (foldl mkApp (mkVar f) ss)

isBound :: Name a -> Bool
isBound = not . isFreeName

getLooseBoundVars :: Match a => a -> [Name a]
getLooseBoundVars = go []
  where
    go inScopeNames t
      | Just x <- isVar t =
          if isBound x && x `notElem` inScopeNames
          then [x]
          else []
      | Just (_, x, body) <- isBinder t = go (x : inScopeNames) body
      | ts <- getChildren t = concatMap (go inScopeNames) ts

