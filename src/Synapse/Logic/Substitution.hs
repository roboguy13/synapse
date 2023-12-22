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
  )
  where

import Prelude hiding (lookup)

import Synapse.Ppr hiding (isEmpty)

import Unbound.Generics.LocallyNameless
import qualified Data.List as List

import GHC.Generics
import Data.Typeable
import Data.Coerce

newtype Substitution a = Substitution [(Name a, a)]
  deriving (Semigroup, Monoid, Show, Generic, Typeable)

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

extend :: Substitution a -> Name a -> a -> Substitution a
extend (Substitution xs) n v = Substitution $ (n, v) : xs

lookup :: Name a -> Substitution a -> Maybe a
lookup n (Substitution xs) = List.lookup n xs

applySubstitution :: Subst a a => Substitution a -> a -> a
applySubstitution (Substitution xs0) = go xs0
  where
    go [] t = t
    go ((x, s):rest) t = subst x s (go rest t)

