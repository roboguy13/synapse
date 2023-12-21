--
-- | These should get merged upstream into the appropriate modules
--

{-# LANGUAGE UndecidableInstances #-}

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

instance Show a => Show1 (Bind a) where
  liftShowsPrec sp sl = showsUnaryWith (liftShowsPrec sp sl) "Bind"
  liftShowList sp sl = showListWith (liftShowsPrec sp sl 0)

