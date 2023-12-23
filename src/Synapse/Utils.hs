{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Synapse.Utils
  where

import Unbound.Generics.LocallyNameless
import GHC.Generics

import Data.Fix

import Control.Monad.State
import Control.Monad.Identity

collapseFresh :: Monad m => FreshMT m a -> FreshM (m a)
collapseFresh = FreshMT . collapseStateT . unFreshMT

collapseStateT :: Monad m => StateT Integer m a -> State Integer (m a)
collapseStateT st = StateT $ \s ->
  let results = evalStateT st s
  in Identity (results, s)

data Void1 a
  deriving (Generic)

deriving instance Show (Void1 a)

instance Alpha (Void1 a)
-- instance Generic b => Subst (Void1 a) b
instance Subst a (Void1 b)

-- | Ensure the lists have the same length
zipWithMaybe :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe _ [] [] = Just []
zipWithMaybe _ [] _ = Nothing
zipWithMaybe _ _ [] = Nothing
zipWithMaybe f (x:xs) (y:ys) = (f x y :) <$> zipWithMaybe f xs ys

