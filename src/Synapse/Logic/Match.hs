{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

module Synapse.Logic.Match
  where

import Prelude hiding (id, (.))

import Control.Category

import Synapse.Logic.Substitution as Substitution
import Synapse.Logic.Injection
import Synapse.Logic.Propagator
import Synapse.Logic.SubstMap
import Synapse.Logic.ConstrEq

import GHC.Generics

import Data.Constraint

import Data.Typeable
import Control.Lens

import Unbound.Generics.LocallyNameless

data NodePair a where
  NodePair :: forall a b. (Match b) => b -> b -> NodePair a

deriving instance Show a => Show (NodePair a)

class Simplify a where
  simplify :: a -> a

class (PartialSemigroup a, Simplify a, Eq a, Plated a, Subst a a, Typeable a, Alpha a) => Match a where
  isConst :: a -> Bool
  mkVar_maybe :: Maybe (Name a -> a)
  isVar :: a -> Maybe (Name a)

  applySubstMap :: SubstMap -> a -> a

  matchConstructor :: a -> a -> Maybe [NodePair a]

  default matchConstructor :: (Generic a, GConstrEq (Rep a), Plated a) => a -> a -> Maybe [NodePair a]
  matchConstructor x y =
    if constrEq x y
    then Just $ zipWith NodePair (children x) (children y)
    else Nothing

