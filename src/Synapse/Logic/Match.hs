{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Synapse.Logic.Match
  where

import Prelude hiding (id, (.))

import Control.Category

import Synapse.Logic.Substitution
import Synapse.Logic.Injection
import Synapse.Logic.SubstMap
import Synapse.Logic.ConstrEq

import GHC.Generics

import Data.Constraint

import Data.Typeable
import Control.Lens

import Unbound.Generics.LocallyNameless

data NodePair a where
  InjPair :: forall a b. Match b => Injection b a -> b -> b -> NodePair a
  SubstMapPair :: forall a b. In b (ContainedTypes a) => b -> b -> NodePair a

class (Eq a, Plated a, Subst a a, Typeable a, Alpha a) => Match a where
  type ContainedTypes a :: [*]

  isConst :: a -> Bool
  mkVar_maybe :: Maybe (Name a -> a)
  isVar :: a -> Maybe (Name a)

  matchConstructor :: a -> a -> Maybe [NodePair a]

  default matchConstructor :: (Generic a, GConstrEq (Rep a), Plated a) => a -> a -> Maybe [NodePair a]
  matchConstructor x y =
    if constrEq x y
    then Just $ zipWith (InjPair id) (children x) (children y)
    else Nothing

  -- isBinder :: a -> Maybe (Int, Name a, a)
  -- isApp :: a -> Maybe (a, a)

  -- mkApp :: a -> a -> a
  -- mkBinder :: Name a -> a -> a

type SubstMap' a = SubstMap (ContainedTypes a)

data MatchSubst a =
  MatchSubst
  { _matchSubstInj :: Substitution a
  , _matchSubstMap :: SubstMap (ContainedTypes a)
  }

makeLenses ''MatchSubst

instance SubstMapSing (ContainedTypes a) => Semigroup (MatchSubst a) where
  MatchSubst x1 y1 <> MatchSubst x2 y2 = MatchSubst (x1 <> x2) (y1 <> y2)

instance SubstMapSing (ContainedTypes a) => Monoid (MatchSubst a) where
  mempty = MatchSubst mempty mempty

-- -- | How to get to the substitution we need. Constructors for the different @Substitution@s to use.
data Path a b where
  PathInj :: Injection a b -> Path a b
  PathSubstMap :: Dict (In b (ContainedTypes a)) -> Path a b


-- data Item a b where
--   InjItem :: Dict (Match b) -> Injection b a -> b -> Item a b
--   SubstMapItem :: Dict (In b (ContainedTypes a)) -> b -> Item a b

pathToLens :: Path a b -> Lens' (MatchSubst a) (Substitution b)
pathToLens (PathInj inj) =
  lens
    (substMap (inject inj) . _matchSubstInj)
    (\matchSubst sbst -> matchSubst & matchSubstInj .~ substMap (\x -> case project inj x of Just r -> r) sbst) -- TODO: Does this make sense?

pathToLens (PathSubstMap Dict) =
  lens
    (getSubst . _matchSubstMap)
    (\matchSubst sbst -> matchSubst & matchSubstMap .~ putSubst sbst (_matchSubstMap matchSubst))

-- pathLookup :: Path a b -> MatchSubst b -> Name a -> Maybe a
-- pathLookup (PathInj inj) mSbst = lookupInj inj (matchSubstInj mSbst)

--
-- pathExtend :: Path a b -> 

-- itemExtend


