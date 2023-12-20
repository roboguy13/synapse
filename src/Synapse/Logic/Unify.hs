--
-- Based on the algorithm from "Functional Unification of Higher-Order Patterns" by Tobias Nipkow
--

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Synapse.Logic.Unify
  where

import Prelude hiding (abs, lookup)

import Synapse.Syntax.Term
import Synapse.Logic.Substitution as Substitution

import Unbound.Generics.LocallyNameless

import Control.Monad.Trans

import Control.Lens.Plated

import Data.Typeable
import Data.Foldable

doOccursCheck :: Bool
doOccursCheck = True

-- data Grammar =
--   Grammar
--   { grammarKeywords :: [String]
--   , grammarBinders :: [String]
--   -- , grammarProductions :: [(String, ProductionRhs)]
--   }
-- type ProductionRhs = [TermSpec]
-- data TermSpec
--   = TermSpec Term
--   | BinderSpecTS BinderSpec

class (Subst a a, Typeable a, Plated a, Alpha a) => Match a where
  isConst :: a -> Bool
  mkVar :: Name a -> a
  isVar :: a -> Maybe (Name a)

  matchConstructor :: a -> a -> Maybe [(a, a)]
  getChildren :: a -> [a]

  isBinder :: a -> Maybe (BinderSort, Name a, a)
  isApp :: a -> Maybe (a, a)

  mkApp :: a -> a -> a
  mkBinder :: Name a -> a -> a

match :: Match a => a -> a -> FreshMT Maybe (Substitution a)
match = generalUnify (\_ _ -> lift Nothing) mempty

unify :: Match a => a -> a -> FreshMT Maybe (Substitution a)
unify = unifySubst mempty

unifySubst :: Match a => Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
unifySubst = generalUnify unifyVar

type VarCase a = Name a -> a -> FreshMT Maybe (Substitution a)

generalUnify :: Match a => VarCase a -> Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
generalUnify varCase sbst s0 t0 =
  case (devar sbst s0, devar sbst t0) of
    (s, t)
      | Just (sortS, x, s) <- isBinder s
      , Just (sortT, y, t) <- isBinder t
      , binderSortId sortS == binderSortId sortT ->
          let t' = if x == y then t else subst y (mkVar x) t
          in
          generalUnify varCase sbst s t'

      | Just (sortS, x, s) <- isBinder s ->
          generalUnify varCase sbst s (mkApp t (mkVar x))

      | Just (sortS, x, t) <- isBinder t ->
          generalUnify varCase sbst (mkApp s (mkVar x)) t

      | otherwise -> nonbinderCases varCase sbst s t
{-# INLINE generalUnify #-}

nonbinderCases :: Match a => VarCase a -> Substitution a -> a -> a -> FreshMT Maybe (Substitution a)
nonbinderCases varCase sbst s t =
  case (strip s, strip t) of
    ((a, ym), (b, tn))
      | Just f <- isVar a, isFreeName f
      , Just g <- isVar b, isFreeName g -> flexFlex f ym g tn sbst

      | Just f <- isVar a, isFreeName f -> flexRigid f ym t sbst

      | Just f <- isVar b, isFreeName f -> flexRigid f ym s sbst

      | otherwise -> rigidRigid a ym b tn sbst

flexFlex :: Match a => Name a -> [a] -> Name a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
flexFlex f ym g zn sbst =
  if f == g
  then flexFlex1 f ym zn sbst
  else flexFlex2 f ym g zn sbst

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

flexRigid :: Match a => Name a -> [a] -> a -> Substitution a -> FreshMT Maybe (Substitution a)
flexRigid f ym t sbst =
  if occursCheck sbst f t
  then lift Nothing
  else proj (map unsafeGetVar ym) (extend sbst f (abs ym t)) t

rigidRigid :: Match a => a -> [a] -> a -> [a] -> Substitution a -> FreshMT Maybe (Substitution a)
rigidRigid a ss b ts sbst =
  if not (a `aeq` b)
  then lift Nothing
  else foldlM (uncurry . unifySubst) sbst (zip ss ts) -- TODO: Use generalUnify

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

unifyVar :: Match a => Name a -> a -> FreshMT Maybe (Substitution a)
unifyVar = undefined

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

