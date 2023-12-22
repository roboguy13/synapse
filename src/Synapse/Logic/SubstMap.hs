{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Synapse.Logic.SubstMap
  where

import Synapse.Logic.Substitution as Substitution

import Unbound.Generics.LocallyNameless

data SubstMap ts where
  Nil :: SubstMap '[]
  Cons :: Substitution b -> SubstMap ts -> SubstMap (b ': ts)


class SubstMapSing ts where
  substMapEmpty :: SubstMap ts

instance SubstMapSing '[] where
  substMapEmpty = Nil

instance SubstMapSing ts => SubstMapSing (a ': ts) where
  substMapEmpty = Cons mempty substMapEmpty

type In a ts = (SubstMapLookup a ts, SubstMapExtend a ts, SubstMapSing ts, GetSubst a ts)

class GetSubst a ts where
  getSubst :: SubstMap ts -> Substitution a
  putSubst :: Substitution a -> SubstMap ts -> SubstMap ts

instance GetSubst a (a ': ts) where
  getSubst (Cons sbst _) = sbst
  putSubst sbst (Cons _ rest) = Cons sbst rest

instance GetSubst a ts => GetSubst a (b ': ts) where
  getSubst (Cons _ rest) = getSubst rest
  putSubst sbst (Cons sbst' rest) = Cons sbst' (putSubst sbst rest)


class SubstMapLookup a ts where
  substMapLookup :: Name a -> SubstMap ts -> Maybe a

instance SubstMapLookup a (a ': ts) where
  substMapLookup x (Cons sbst _) = Substitution.lookup x sbst

instance SubstMapLookup a ts => SubstMapLookup a (b ': ts) where
  substMapLookup x (Cons _ rest) = substMapLookup x rest


class SubstMapExtend a ts where
  substMapExtend :: Name a -> a -> SubstMap ts -> SubstMap ts

instance SubstMapExtend a (a ': ts) where
  substMapExtend x t (Cons sbst rest) = Cons (extend sbst x t) rest

instance SubstMapExtend a ts => SubstMapExtend a (b ': ts) where
  substMapExtend x t (Cons sbst rest) = Cons sbst (substMapExtend x t rest)

instance Semigroup (SubstMap ts) where
  Nil <> Nil = Nil
  Cons sbst rest <> Cons sbst' rest' = Cons (sbst <> sbst') (rest <> rest')

instance SubstMapSing ts => Monoid (SubstMap ts) where
  mempty = substMapEmpty


