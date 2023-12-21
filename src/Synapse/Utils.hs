{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Synapse.Utils
  where

import Unbound.Generics.LocallyNameless
import GHC.Generics

import Data.Fix

data Void1 a
  deriving (Generic)

deriving instance Show (Void1 a)

instance Alpha (Void1 a)
-- instance Generic b => Subst (Void1 a) b
instance Subst a (Void1 b)

