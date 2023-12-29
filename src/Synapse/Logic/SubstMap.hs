{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Synapse.Logic.SubstMap
  (SubstMap
  ,substLens
  ,hlistEmpty
  ,SubstMapCells
  ,convertSubstMap
  -- ,substMapEmpty
  )
  where

import Synapse.Logic.Substitution as Substitution
import Synapse.Logic.Propagator

import Control.Monad.ST
import Control.Monad.ST.Class

import Unbound.Generics.LocallyNameless

import Control.Lens
import Data.Void
import Data.Fix
import Data.Functor.Product
import Data.Functor.Compose
import Data.Functor.Const

import Data.Type.Equality
import Type.Reflection
import Data.Typeable
import Data.Default

data HList f where
  Nil :: HList f
  Cons :: Typeable a => f a -> HList f -> HList f

substLens :: forall f a. (Default (f a), Typeable a) =>
  Lens' (HList f) (f a)
substLens =
  lens get set
  where
    get :: HList f -> f a
    get Nil = def
    get (Cons @b x xs) =
      case testEquality (TypeRep @a) (TypeRep @b) of
        Just Refl -> x
        Nothing -> get xs

    set :: HList f -> f a -> HList f
    set Nil sbst = Cons sbst Nil
    set (Cons @b x xs) sbst =
      case testEquality (TypeRep @a) (TypeRep @b) of
        Just Refl -> Cons sbst xs
        Nothing -> Cons x (set xs sbst)

hlistEmpty :: HList f
hlistEmpty = Nil

type SubstMapCells s = HList (SubstCells s)
type SubstMap = HList Substitution

convertSubstMap :: MonadST m => SubstMapCells m -> m SubstMap
convertSubstMap Nil = pure Nil
convertSubstMap (Cons x xs) = do
  x' <- toSubstitution x
  xs' <- convertSubstMap xs
  pure $ Cons x' xs'

-- instance Semigroup SubstMap where
--   Nil <> ys       = ys
--   Cons x xs <> ys = ys & substLens %~ (<> x)

-- instance Monoid SubstMap where
--   mempty = Nil

-- type family ContainedTypes a :: [*]
--
-- type instance ContainedTypes Void = '[]
-- type instance ContainedTypes () = '[]
-- type instance ContainedTypes [a] = ContainedTypes a
-- -- type instance ContainedTypes (Fix f) = ContainedTypes (f (Fix f))
-- -- type instance ContainedTypes (Compose f g a) = ContainedTypes (f (g a))
--
-- data SubstMap ts where
--   Nil :: SubstMap '[]
--   Cons ::
--      SubstMap (ContainedTypes b) ->    -- There's a sort of transitive containment
--      Substitution b -> SubstMap ts -> SubstMap (b ': ts)
--
-- data Elem a ts where
--   Here :: Elem a (a ': ts)
--   There :: Elem a ts -> Elem a (b ': ts)
--
--   Descend ::
--     -- | Used properly, this case should imply @(Not (Elem a ts), Not (a :~: b))@, but this isn't
--     -- encoded in the type
--     -- TODO: Is there a better representation that guarantees this by
--     -- construction?
--     Elem a (ContainedTypes b) -> Elem a (b ': ts)
--
-- elemToLens :: Elem a ts -> Lens' (SubstMap ts) (Substitution a)
-- elemToLens Here = lens (\(Cons _ x _) -> x) (\(Cons t _ xs) x -> Cons t x xs)
-- elemToLens (There p) =
--   lens (\(Cons _ _ xs) -> xs ^. elemToLens p) (\(Cons t x xs) y -> Cons t x (xs & elemToLens p .~ y))
-- elemToLens (Descend p) =
--   lens (\(Cons t _ _) -> t ^. elemToLens p) (\(Cons t x xs) y -> Cons (t & elemToLens p .~ y) x xs)
--
-- contained :: Elem a ts -> Elem x (ContainedTypes a) -> Elem x ts
-- contained Here q = Descend q
-- contained (There p) q = There (contained p q)
--
-- elemCompose :: forall a b c.
--   Elem c (ContainedTypes b) ->
--   Elem b (ContainedTypes a) ->
--   Elem c (ContainedTypes a)
-- elemCompose p q = contained q p
--
-- class SubstMapSing ts where
--   substMapEmpty :: SubstMap ts
--
-- instance SubstMapSing '[] where
--   substMapEmpty = Nil
--
-- instance (SubstMapSing (ContainedTypes a), SubstMapSing ts) => SubstMapSing (a ': ts) where
--   substMapEmpty = Cons mempty mempty substMapEmpty
--
-- type In a ts = (SubstMapLookup a ts, SubstMapExtend a ts, SubstMapSing ts, GetSubst a ts)
--
-- class GetSubst a ts where
--   getSubst :: SubstMap ts -> Substitution a
--   putSubst :: Substitution a -> SubstMap ts -> SubstMap ts
--
-- instance GetSubst a (a ': ts) where
--   getSubst (Cons _ sbst _) = sbst
--   putSubst sbst (Cons m _ rest) = Cons m sbst rest
--
-- instance GetSubst a ts => GetSubst a (b ': ts) where
--   getSubst (Cons _ _ rest) = getSubst rest
--   putSubst sbst (Cons m sbst' rest) = Cons m sbst' (putSubst sbst rest)
--
--
-- class SubstMapLookup a ts where
--   substMapLookup :: Name a -> SubstMap ts -> Maybe a
--
-- instance SubstMapLookup a (a ': ts) where
--   substMapLookup x (Cons _ sbst _) = Substitution.lookup x sbst
--
-- instance SubstMapLookup a ts => SubstMapLookup a (b ': ts) where
--   substMapLookup x (Cons _ _ rest) = substMapLookup x rest
--
--
-- class SubstMapExtend a ts where
--   substMapExtend :: Name a -> a -> SubstMap ts -> SubstMap ts
--
-- instance SubstMapExtend a (a ': ts) where
--   substMapExtend x t (Cons m sbst rest) = Cons m (extend sbst x t) rest
--
-- instance SubstMapExtend a ts => SubstMapExtend a (b ': ts) where
--   substMapExtend x t (Cons m sbst rest) = Cons m sbst (substMapExtend x t rest)
--
-- instance Semigroup (SubstMap ts) where
--   Nil <> Nil = Nil
--   Cons m1 sbst rest <> Cons m2 sbst' rest' = Cons (m1 <> m2) (sbst <> sbst') (rest <> rest')
--
-- instance SubstMapSing ts => Monoid (SubstMap ts) where
--   mempty = substMapEmpty
--
