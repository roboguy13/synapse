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

class Ppr a where
  ppr :: a -> Doc

instance Ppr Int where ppr = text . show
instance Ppr (Name a) where ppr = text . show
    
(<.>) = (Text.PrettyPrint.HughesPJ.<>)

pprRender :: Ppr a => a -> String
pprRender = render . ppr

