--
-- Terms in the object language. These are represented as s-expressions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Synapse.Syntax.Term
  where

import Synapse.Ppr
import Synapse.Logic.Unify
import Synapse.Logic.SubstMap
import Synapse.Logic.Substitution
import Synapse.Orphans
import Synapse.Utils
import Synapse.Logic.ConstrEq
import Synapse.Logic.Propagator

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe
import Unbound.Generics.LocallyNameless.Bind

import GHC.Generics
import Data.Typeable

import Control.Lens hiding ((<.>))
import Control.Applicative
import Control.Monad

import Text.Show.Deriving

import Type.Reflection (pattern TypeRep)
import Data.Type.Equality

import Data.Coerce
import Data.Void
import Data.Fix
import Data.Functor.Product
import Data.Functor.Compose

import Unsafe.Coerce

#include "src/Synapse/SubstUtils.hs"

newtype BinderSort = BinderSort Int
  deriving (Show, Generic, Typeable)

binderSortId :: BinderSort -> Int
binderSortId (BinderSort i) = i

data BinderSpec =
  BinderSpec [TermName] Term
  deriving (Show, Generic, Typeable)

type TermNameX x = Name (TermX x)

type Term = TermX Void
type TermName = TermNameX Void

type SubstTerm = TermX FSubstTerm
type FSubstTerm = Fix (Compose TermX (Product TermX (Const (Substitution Term))))

data TermX x
  = Symbol String
  | IntLit Int
  | Var (Name (TermX x))
  | App (TermX x) [TermX x]
  | Binder BinderSort (Bind TermName (TermX x))
  | TermX x
  deriving (Show, Generic, Typeable)

-- class HasTerms a where
--   terms :: Applicative f => (forall x. TermX x -> f (TermX x)) -> a -> f a

class RenameTerms a where
  renameTerms :: [(Name Term, Name Term)] -> a -> a
  termFVs :: a -> [Name Term]

instance (Alpha a, Subst (TermX a) a, Typeable a) => RenameTerms (TermX a) where
  renameTerms xs = substs $ map (fmap Var) (coerce xs :: [(Name (TermX a), Name (TermX a))])
  termFVs t = coerce (toListOf fv t :: [Name (TermX a)])

-- class FreshenTerms a where
--   freshenTerms :: 

-- instance HasTerms (TermX x) where
--   terms = id

-- class HasSubstTerms a where
--   substTerms :: Traversal' a SubstTerm
--
-- instance HasSubstTerms SubstTerm where
--   substTerms = id

instance (Typeable a, Alpha a) => Eq (TermX a) where
  (==) = aeq

instance Functor TermX where
  fmap _ (Symbol s) = Symbol s
  fmap _ (IntLit i) = IntLit i
  fmap _ (Var x) = Var $ coerce x
  fmap f (App a b) = App (fmap f a) (map (fmap f) b)
  fmap f (Binder bSort (B x body)) = -- TODO: Is this safe?
    Binder bSort $ B x $ fmap f body
  fmap f (TermX x) = TermX (f x)

instance Applicative TermX where
  pure = TermX
  (<*>) = ap

instance Monad TermX where
  return = pure
  Symbol s >>= _ = Symbol s
  IntLit i >>= _ = IntLit i
  Var x >>= _ = Var $ coerce x
  App a b >>= f = App (a >>= f) (map (>>= f) b)
  Binder bSort (B x body) >>= f = Binder bSort (B x (body >>= f))
  TermX x >>= f = f x

pattern TSubst :: SubstTerm -> Substitution Term -> SubstTerm
pattern TSubst t s = TermX (Fix (Compose (TermX (Pair t (Const s)))))

pattern FTSubst :: SubstTerm -> Substitution Term -> FSubstTerm
pattern FTSubst t s = Fix (Compose (TermX (Pair t (Const s))))

newtype TermSpecAlt = TermSpecAlt { unTermSpecAlt :: Term }
  deriving (Show, Generic, Eq)

data TermSpec = TermSpec [String] [TermSpecAlt]
  deriving (Show, Generic, Eq)

instance RenameTerms TermSpecAlt where
  renameTerms xs (TermSpecAlt t) = TermSpecAlt $ renameTerms xs t
  termFVs (TermSpecAlt t) = termFVs t

instance RenameTerms TermSpec where
  renameTerms xs (TermSpec str alts) = TermSpec str (map (renameTerms xs) alts)
  termFVs (TermSpec _ alts) = concatMap termFVs alts

-- instance HasTerms TermSpecAlt where
--   terms f (TermSpecAlt t) = TermSpecAlt <$> f t
--
-- instance HasTerms TermSpec where
--   terms f (TermSpec strs alts) =
--     TermSpec strs <$> traverse (terms f) alts

type Grammar = [TermSpec]

instance Alpha TermSpecAlt
instance Alpha TermSpec

instance Simplify TermSpecAlt where
  simplify = id

instance Simplify TermSpec where
  simplify = id
  -- simplify (TermSpec xs alts) = TermSpec xs (map simplify alts)

instance Plated TermSpec where
  plate _ = pure


instance Plated TermSpecAlt where
  plate f (TermSpecAlt t) = TermSpecAlt <$> plate (fmap unTermSpecAlt . f . TermSpecAlt) t

instance Subst TermSpec TermSpec
instance (Typeable a, Alpha a, Subst TermSpec a) => Subst TermSpec (TermX a)
instance Subst TermSpec Void
instance Subst TermSpec TermSpecAlt
instance Subst TermSpec BinderSort

instance Subst TermSpecAlt TermSpecAlt
instance (Typeable a, Alpha a, Subst TermSpecAlt a) => Subst TermSpecAlt (TermX a)
instance Subst TermSpecAlt Void
instance Subst TermSpecAlt BinderSort

dropX :: TermX a -> Maybe Term
dropX (TermX {}) = Nothing
dropX (Symbol s) = Just $ Symbol s
dropX (IntLit i) = Just $ IntLit i
dropX (Var x) = Just $ Var $ coerce x
dropX (App a b) = do
  a' <- dropX a
  b' <- traverse dropX b
  pure $ App a' b'
dropX (Binder bSort (B x body)) = do
  body' <- dropX body
  pure $ Binder bSort (B x body')

retagTerm :: Term -> TermX a
retagTerm = unsafeCoerce -- NOTE: This should be safe

convertTerm :: (a -> Term) -> TermX a -> Term
convertTerm f = \case
  Symbol s -> Symbol s
  IntLit i -> IntLit i
  Var x -> Var $ coerce x
  App a b -> App (convertTerm f a) (map (convertTerm f) b)
  Binder bSort (B x body) -> Binder bSort (B x (convertTerm f body))
  TermX x -> f x

convertSubstTerm :: SubstTerm -> Term
convertSubstTerm = convertTerm go
  where
    go :: FSubstTerm -> Term
    go (FTSubst t s) = applySubstitution s (convertSubstTerm t)

instance Simplify SubstTerm where
  simplify = retagTerm . convertSubstTerm

instance Simplify Term where
  simplify = id

instance Ppr Grammar where
  ppr = vcat . map go
    where
      go (TermSpec names specAlts) =
        sep (punctuate (text ",") (map (text . ('?':)) names))
          <+> text "::="
          $$ nest 2 (vcat (mapHead (text " " <+>) (mapTail (text "|" <+>) (map ppr specAlts))))

      mapTail f [] = []
      mapTail f (x:xs) = x : map f xs

      mapHead f [] = []
      mapHead f (x:xs) = f x : xs

instance Ppr TermSpecAlt where
  ppr (TermSpecAlt alt) = ppr alt

instance Ppr TermSpec where
  ppr (TermSpec _ xs) =
    text "["
    <.>
    hsep (punctuate (text ",") (map ppr xs))
    <.>
    text "]"

-- | Find the TermSpec corresponding to the given variable name
lookupTermSpec :: Grammar -> String -> TermSpec
lookupTermSpec (spec@(TermSpec names _) : rest) name =
  if getVarName name `elem` names
  then spec
  else lookupTermSpec rest name

-- Get the part before the underscore. So, ?e_10 gives back the ?e name
getVarName :: String -> String
getVarName = takeWhile (/= '_')

-- mkTermSpec :: [Term] -> TermSpec
-- mkTermSpec = TermSpec . coerce

termSpecAltSplit :: TermSpecAlt -> Maybe (String, [Term])
termSpecAltSplit (TermSpecAlt (App (Symbol c) args)) = Just (c, args)
termSpecAltSplit _ = Nothing

pattern Binder' bnd = Binder (BinderSort 0) bnd

instance (Typeable x, Alpha x) => Plated (TermX x) where
  plate f (Symbol str) = pure $ Symbol str
  plate f (IntLit i) = pure $ IntLit i
  plate f (Var x) = pure $ Var x
  plate f (App t args) = App <$> f t <*> traverse f args
  plate f (Binder bSort bnd) =
    let (x, body) = unsafeUnbind bnd
    in
    Binder bSort . bind x <$> f body

instance Ppr x => Ppr (TermX x) where
  ppr (Symbol sym) = text sym
  ppr (IntLit i) = ppr i
  ppr (Var x) = text "?" <.> ppr x
  ppr (App f args) =
    parens (hsep (map ppr (f : args)))
  ppr (TermX x) = ppr x

  -- TODO: Write this case
  -- ppr (Binder (BinderSort pprBinder) params body) = pprBinder params body

instance Alpha BinderSort
instance (Typeable a, Alpha a) => Alpha (TermX a)

instance Subst (TermX x) BinderSort

instance Subst (TermX a) Void
-- instance Subst (TermX Void) Void

instance forall x b. (Typeable x, Typeable b, Subst (TermX x) b, Alpha b) => Subst (TermX x) (TermX b) where
  isvar (Var x) = do
    Refl <- testEquality (TypeRep @x) (TypeRep @b)
    Just $ SubstName $ coerce x
  isvar _ = Nothing

instance (Subst (TermX a) b) => Subst (TermX a) (Substitution b)
-- instance (Simplify (TermX x), Subst (TermX x) x, Typeable x, Alpha x) => Match (TermX x) where
--   isConst (Symbol _) = True
--   isConst _ = False
--
--   mkVar_maybe = Just $ Var . coerce
--
--   isVar (Var x) = Just $ coerce x
--   isVar _ = Nothing
--
--   matchConstructor (TermX x) y = undefined
--   matchConstructor x y =
--     if constrEq x y
--     then Just $ zipWith NodePair (children x) (children y)
--     else Nothing
--
--   applySubstMap sbst = applySubstitution (sbst ^. substLens) --applySubstitution . _matchSubstInj


  -- matchConstructor x y = _

  -- getChildren = children
  --
  -- isBinder (Binder bSort bnd) =
  --   let (x, body) = unsafeUnbind bnd
  --   in Just (binderSortId bSort, coerce x, body)
  -- isBinder _ = Nothing
  --
  -- isApp (App x y) =
  --   let (priorArgs, lastArg) = splitLast y
  --   in
  --   Just (App x priorArgs, lastArg)
  -- isApp _ = Nothing
  --
  -- mkApp (App (App x y) z) w = mkApp (App x (y ++ z)) w
  -- mkApp (App x y) w = App x (y ++ [w])
  -- mkApp x w = App x [w]
  --
  -- mkBinder = (Binder (BinderSort 0) .) . bind . coerce

splitLast :: [a] -> ([a], a)
splitLast [x] = ([], x)
splitLast (x:xs) =
  let (rest, z) = splitLast xs
  in
  (x:rest, z)

deriveShow1 ''TermX

-- instance (Subst (TermX a) b, Alpha b) => Subst (TermX a) (TermX b)

-- SUBST_INSTANCES1(TermX)

instance Match Term where
  isConst (Symbol _) = True
  isConst _ = False

  mkVar_maybe = Just $ Var . coerce

  isVar (Var x) = Just $ coerce x
  isVar _ = Nothing

  applySubstMap sbst = applySubstitution (sbst ^. substLens)

instance Match SubstTerm where
  isConst (Symbol _) = True
  isConst _ = False

  mkVar_maybe = Just $ Var . coerce

  isVar (Var x) = Just $ coerce x
  isVar _ = Nothing

  -- matchConstructor (TermX x) y =
  --   Just [NodePair (getCompose (unFix x)) y]
  matchConstructor x y =
    if constrEq x y
    then Just $ zipWith NodePair (children x) (children y)
    else Nothing

  applySubstMap sbst = applySubstitution (sbst ^. substLens) --applySubstitution . _matchSubstInj

instance Match TermSpecAlt where
  isConst = isConst . unTermSpecAlt
  mkVar_maybe = fmap ((TermSpecAlt .) . (. coerce)) mkVar_maybe
  isVar = coerce . isVar . unTermSpecAlt

  applySubstMap substMap = coerce (applySubstMap substMap :: Term -> Term)

instance Match TermSpec where
  isConst _ = False
  mkVar_maybe = Nothing
  isVar _ = Nothing
  matchConstructor (TermSpec xs alts) (TermSpec ys alts') = do
    guard (xs == ys)
    zipWithMaybe NodePair alts alts'

  applySubstMap substMap (TermSpec xs ys) =
    TermSpec xs (map (applySubstMap substMap) ys)

termMatchesSpec :: Term -> TermSpec -> Maybe (TermSpecAlt, SubstMap)
termMatchesSpec t (TermSpec _ spec) = go spec
  where
    go []                                 = Nothing
    go (alt@(TermSpecAlt altSpec) : alts) = fmap (alt, ) (match t altSpec) <|> go alts

instance PartialSemigroup SubstTerm where
  x@TSubst {} <<>> y@TSubst {}
    | x `aeq` y = pure x
    | otherwise = Inconsistent

  x0 <<>> y0 =
    case (x0, y0) of
      (TSubst x sbst, y) -> go sbst x y
      (x, TSubst y sbst) -> go sbst y x
      (x, y)
        | x `aeq` y -> pure x
        | otherwise -> Inconsistent
    where
      go sbst x y =
        let x' = applySubstitution (substMap retagTerm sbst) x
        in
        x' <<>> y

instance PartialSemigroup Term where
  x <<>> y =
    if x `aeq` y
    then Known x
    else Inconsistent

instance PartialSemigroup TermSpec where
  x <<>> y =
    if x `aeq` y
    then Known x
    else Inconsistent

instance PartialSemigroup TermSpecAlt where
  x <<>> y =
    if x `aeq` y
    then Known x
    else Inconsistent

