module Synapse.Ppr
  (module Text.PrettyPrint.HughesPJ
  ,Ppr (..)
  ,pprRender
  )
  where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import Unbound.Generics.LocallyNameless

class Ppr a where
  ppr :: a -> Doc

instance Ppr Int where ppr = text . show
instance Ppr (Name a) where ppr = text . show

pprRender :: Ppr a => a -> String
pprRender = render . ppr
