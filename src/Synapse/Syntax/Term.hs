--
-- Terms in the object language. These are represented as s-expressions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Synapse.Syntax.Term
  where

import Synapse.Ppr
import Synapse.Logic.Unify

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

import GHC.Generics
import Data.Typeable

import Control.Lens.Plated
import Control.Applicative

import Data.Coerce

newtype BinderSort = BinderSort Int
  deriving (Show, Generic, Typeable)

binderSortId :: BinderSort -> Int
binderSortId (BinderSort i) = i

data BinderSpec =
  BinderSpec [TermName] Term
  deriving (Show, Generic, Typeable)

type TermName = Name Term

newtype TermSpecAlt = TermSpecAlt Term

newtype TermSpec = TermSpec [TermSpecAlt]

mkTermSpec :: [Term] -> TermSpec
mkTermSpec = TermSpec . coerce

termSpecAltSplit :: TermSpecAlt -> Maybe (String, [Term])
termSpecAltSplit (TermSpecAlt (App (Symbol c) args)) = Just (c, args)
termSpecAltSplit _ = Nothing

termMatchesSpec :: Term -> TermSpec -> Maybe (TermSpecAlt, Substitution Term)
termMatchesSpec t (TermSpec spec) = go spec
  where
    go []                                 = Nothing
    go (alt@(TermSpecAlt altSpec) : alts) = sequenceA (fmap (alt, ) runFreshMT (match t altSpec)) <|> go alts

data Term
  = Symbol String
  | IntLit Int
  | Var TermName
  | App Term [Term]
  | Binder BinderSort (Bind TermName Term)
  deriving (Show, Generic, Typeable)

pattern Binder' bnd = Binder (BinderSort 0) bnd

instance Plated Term where
  plate f (Symbol str) = pure $ Symbol str
  plate f (IntLit i) = pure $ IntLit i
  plate f (Var x) = pure $ Var x
  plate f (App t args) = App <$> f t <*> traverse f args
  plate f (Binder bSort bnd) =
    let (x, body) = unsafeUnbind bnd
    in
    Binder bSort . bind x <$> f body

instance Ppr Term where
  ppr (Symbol sym) = text sym
  ppr (IntLit i) = ppr i
  ppr (Var x) = ppr x
  ppr (App f args) =
    parens (hsep (map ppr (f : args)))

  -- TODO: Write this case
  -- ppr (Binder (BinderSort pprBinder) params body) = pprBinder params body

instance Alpha BinderSort
instance Alpha Term

instance Subst Term BinderSort

instance Subst Term Term where
  isvar (Var x) = Just $ SubstName x
  isvar _ = Nothing

instance Match Term where
  isConst (Symbol _) = True
  isConst _ = False

  mkVar = Var

  isVar (Var x) = Just x
  isVar _ = Nothing

  getChildren = children

  isBinder (Binder bSort bnd) =
    let (x, body) = unsafeUnbind bnd
    in Just (binderSortId bSort, x, body)
  isBinder _ = Nothing

  isApp (App x y) =
    let (priorArgs, lastArg) = splitLast y
    in
    Just (App x priorArgs, lastArg)
  isApp _ = Nothing

  mkApp (App (App x y) z) w = mkApp (App x (y ++ z)) w
  mkApp (App x y) w = App x (y ++ [w])
  mkApp x w = App x [w]

  mkBinder = (Binder (BinderSort 0) .) . bind

splitLast :: [a] -> ([a], a)
splitLast [x] = ([], x)
splitLast (x:xs) =
  let (rest, z) = splitLast xs
  in
  (x:rest, z)

