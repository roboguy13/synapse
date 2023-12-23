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
{-# LANGUAGE TypeOperators #-}

module Synapse.Logic.Match
  where

import Prelude hiding (id, (.))

import Control.Category

import Synapse.Logic.Substitution as Substitution
import Synapse.Logic.Injection
import Synapse.Logic.SubstMap
import Synapse.Logic.ConstrEq

import GHC.Generics

import Data.Constraint

import Data.Typeable
import Control.Lens

import Unbound.Generics.LocallyNameless

data NodePair a where
  NodePair :: forall a b. Match b => Path a b -> b -> b -> NodePair a

-- | How to get to the substitution we need. Constructors for the different @Substitution@s to use.
data Path a b where
  PathInj :: forall a b. Injection b a -> Path a b
  PathSubstMap :: forall a b. Elem b (ContainedTypes a) -> Path a b

idPath :: Path a a
idPath = PathInj id

instance Category Path where
  id = PathInj id
  PathInj i . PathInj j = PathInj (j . i)
  PathSubstMap @b @c p . PathSubstMap @a @_ q = PathSubstMap (elemCompose @a @b @c p q)

class (Eq a, Plated a, Subst a a, Typeable a, Alpha a) => Match a where
  -- type ContainedTypes a :: [*]

  isConst :: a -> Bool
  mkVar_maybe :: Maybe (Name a -> a)
  isVar :: a -> Maybe (Name a)

  applyMatchSubst :: MatchSubst a -> a -> a

  matchConstructor :: a -> a -> Maybe [NodePair a]

  default matchConstructor :: (ContainedTypes a ~ '[], Generic a, GConstrEq (Rep a), Plated a) => a -> a -> Maybe [NodePair a]
  matchConstructor x y =
    if constrEq x y
    then Just $ zipWith (NodePair (PathInj id)) (children x) (children y)
    else Nothing

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

pathToLens :: Path a b -> Lens' (MatchSubst a) (Substitution b)
pathToLens (PathInj inj) =
  lens
    (substMap (\x -> case project inj x of Just r -> r) . _matchSubstInj) -- TODO: Does this make sense?
    (\matchSubst sbst -> matchSubst & matchSubstInj .~ substMap (inject inj) sbst)

pathToLens (PathSubstMap p) =
  lens
    (view (matchSubstMap . elemToLens p))
    (\matchSubst sbst -> matchSubst & (matchSubstMap . elemToLens p) .~ sbst)
    -- (\matchSubst sbst -> matchSubst & (matchSubstMap._) .~ (_matchSubstMap matchSubst))

-- lensToPath

pathLookup :: Path a b -> MatchSubst a -> Name b -> Maybe b
pathLookup path mSbst x = Substitution.lookup x (mSbst ^. pathToLens path)

pathExtend :: Path a b -> MatchSubst a -> Name b -> b -> MatchSubst a
pathExtend path mSbst x t =
  mSbst & pathToLens path %~ \sbst -> extend sbst x t

