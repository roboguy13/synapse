{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Synapse.Ppr
  (module Text.PrettyPrint.HughesPJ
  ,Ppr (..)
  ,pprRender
  ,(<.>)
  )
  where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import qualified Text.PrettyPrint.HughesPJ
import Unbound.Generics.LocallyNameless

import Data.Void
import Data.Fix
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Const

class Ppr a where
  ppr :: a -> Doc

instance Ppr Int where ppr = text . show
instance Ppr (Name a) where ppr = text . show
instance Ppr Void where ppr = \case

instance Ppr (f (Fix f)) => Ppr (Fix f) where ppr (Fix x) = ppr x
instance Ppr (f (g a)) => Ppr (Compose f g a) where ppr (Compose x) = ppr x
instance Ppr a => Ppr (Const a b) where ppr (Const x) = ppr x
instance (Ppr (f a), Ppr (g a)) => Ppr (Product f g a) where ppr (Pair x y) = ppr x <.> ppr y -- NOTE: This is for something very specific (displaying explicit substitutions). Maybe I should rework this?
    
(<.>) = (Text.PrettyPrint.HughesPJ.<>)

pprRender :: Ppr a => a -> String
pprRender = render . ppr

