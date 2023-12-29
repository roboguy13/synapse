{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Synapse.Logic.Substitution
  (Substitution
  ,substMap
  ,isEmpty
  ,oneSubst
  ,extend
  ,lookup
  ,applySubstitution
  ,SubstCells
  ,emptySubstCells
  ,mkUnknownSubstCell
  ,watchSubstCell
  ,writeSubstCell
  ,lookupSubstCell
  ,toSubstitution
  )
  where

import Prelude hiding (lookup)

import Synapse.Ppr hiding (isEmpty)
import Synapse.Logic.Propagator

import Control.Monad.ST
import Control.Monad.ST.Class

import Data.Default

import Unbound.Generics.LocallyNameless
import qualified Data.List as List

import Data.Functor

import GHC.Generics
import Data.Typeable
import Data.Coerce

newtype Substitution a = Substitution [(Name a, a)]
  deriving (Semigroup, Monoid, Show, Generic, Typeable)

instance Default (Substitution a) where
  def = mempty

-- | NOTE: Be careful using this
substMap :: (a -> b) -> Substitution a -> Substitution b
substMap f (Substitution xs) = Substitution $ map go xs
  where
    go (x, t) = (coerce x, f t)

instance (Show a, Typeable a, Alpha a) => Alpha (Substitution a)

instance Ppr a => Ppr (Substitution a) where
  ppr (Substitution xs0) = text "[" <.> hsep (punctuate (text ",") (map go xs0)) <.> text "]"
    where
      go (x, t) = text "?" <.> ppr x <+> text ":=" <+> ppr t

isEmpty :: Substitution a -> Bool
isEmpty (Substitution []) = True
isEmpty _                 = False

oneSubst :: Name a -> a -> Substitution a
oneSubst n v = Substitution [(n, v)]

extend :: Name a -> a -> Substitution a -> Substitution a
extend n v (Substitution xs) = Substitution $ (n, v) : xs

lookup :: Name a -> Substitution a -> Maybe a
lookup n (Substitution xs) = List.lookup n xs

applySubstitution :: Subst a a => Substitution a -> a -> a
applySubstitution (Substitution xs0) = go xs0
  where
    go [] t = t
    go ((x, s):rest) t = subst x s (go rest t)

newtype SubstCells m a = SubstCells [(Name a, Cell m a)]

emptySubstCells :: SubstCells m a
emptySubstCells = SubstCells []

instance Default (SubstCells m a) where def = emptySubstCells

mkUnknownSubstCell :: MonadST m => SubstCells m a -> Name a -> m (Cell m a, SubstCells m a)
mkUnknownSubstCell (SubstCells xs) n = do
  cell <- mkUnknown
  pure (cell, SubstCells ((n, cell) : xs))

watchSubstCell :: (MonadST m, PartialSemigroup a) =>
  SubstCells m a -> Name a -> (a -> m ()) -> m ()
watchSubstCell (SubstCells ((x, cell):rest)) name k
  | x == name = watch cell go
  | otherwise = watchSubstCell (SubstCells rest) name k
  where
    go (Known v) = k v
    go _ = pure ()

writeSubstCell :: (MonadST m, PartialSemigroup a) =>
  SubstCells m a -> Name a -> a -> m (SubstCells m a)
writeSubstCell (SubstCells []) name v = do
  cell <- mkKnown v
  pure $ SubstCells [(name, cell)]
writeSubstCell sc@(SubstCells ((x, cell):rest)) name v
  | x == name = writeCell_ cell v $> sc
  | otherwise = cons (x, cell) <$> writeSubstCell (SubstCells rest) name v
    where
      cons p (SubstCells s) = SubstCells (p : s)

lookupSubstCell :: Name a -> SubstCells m a -> Maybe (Cell m a)
lookupSubstCell _ (SubstCells []) = Nothing
lookupSubstCell name (SubstCells ((x, cell):rest))
  | x == name = Just cell
  | otherwise = lookupSubstCell name (SubstCells rest)

toSubstitution :: MonadST m => SubstCells m a -> m (Substitution a)
toSubstitution (SubstCells []) = pure mempty
toSubstitution (SubstCells ((x, cell) : rest)) = do
  restSubst <- toSubstitution (SubstCells rest)
  pure $ Substitution undefined

