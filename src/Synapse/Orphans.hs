--
-- | These should get merged upstream into the appropriate modules
--

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Synapse.Orphans
  where

import Unbound.Generics.LocallyNameless
import Data.Void

import Text.Show
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Const
import Data.Fix

instance Alpha Void
instance (Show1 f, Alpha (f (Fix f))) => Alpha (Fix f)
instance (Show1 f, Show1 g, Show a, Alpha (f (g a))) => Alpha (Compose f g a)
instance (Show1 f, Show1 g, Show a, Alpha (f a), Alpha (g a)) => Alpha (Product f g a)
instance (Show a, Alpha a) => Alpha (Const a b)

instance Subst a (f (Fix f)) => Subst a (Fix f)
instance Subst a (f (g b)) => Subst a (Compose f g b)
instance (Subst a (f b), Subst a (g b)) => Subst a (Product f g b)
instance (Subst a b) => Subst a (Const b c)

instance Show a => Show1 (Bind a) where
  liftShowsPrec _ _ = undefined --showsPrec
  -- liftShowsPrec sp sl = showsUnaryWith (liftShowsPrec sp sl) "Bind"
  liftShowList sp sl = undefined -- showListWith (liftShowsPrec sp sl 0)

instance Show1 Name where
  liftShowsPrec _ _ = showsPrec
  liftShowList sp sl = showList --showListWith (liftShowsPrec sp sl 0)

