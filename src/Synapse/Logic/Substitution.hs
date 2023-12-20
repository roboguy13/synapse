{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Synapse.Logic.Substitution
  (Substitution
  ,isEmpty
  ,extend
  ,lookup
  ,applySubstitution
  )
  where

import Prelude hiding (lookup)
import Unbound.Generics.LocallyNameless
import qualified Data.List as List

newtype Substitution a = Substitution [(Name a, a)]
  deriving (Semigroup, Monoid)

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

