{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Synapse.Logic.Substitution
  (Substitution
  ,extend
  ,lookup
  )
  where

import Prelude hiding (lookup)
import Unbound.Generics.LocallyNameless
import qualified Data.List as List

newtype Substitution a = Substitution [(Name a, a)]
  deriving (Semigroup, Monoid)

oneSubst :: Name a -> a -> Substitution a
oneSubst n v = Substitution [(n, v)]

extend :: Substitution a -> Name a -> a -> Substitution a
extend (Substitution xs) n v = Substitution $ (n, v) : xs

lookup :: Name a -> Substitution a -> Maybe a
lookup n (Substitution xs) = List.lookup n xs

