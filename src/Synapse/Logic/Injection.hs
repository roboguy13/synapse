{-# LANGUAGE RankNTypes #-}

module Synapse.Logic.Injection
  (Injection
  ,mkInjection
  ,inject
  ,project
  ,injectionPrism
  ,injectSubst
  ,oneSubstInj
  ,lookupInj
  ,extendInj
  )
  where

import Prelude hiding (id, (.))

import Synapse.Logic.Substitution as Substitution
import Synapse.Ppr

import Data.Typeable
import Data.Coerce

import Control.Category
import Control.Monad

import Control.Lens

import Unbound.Generics.LocallyNameless

data Injection a b =
  Injection
    { inject :: a -> b
    , project :: b -> Maybe a
    }

injectionPrism :: Injection a b -> Prism' b a
injectionPrism inj = prism' (inject inj) (project inj)

mkInjection :: (a -> b) -> (b -> Maybe a) -> Injection a b
mkInjection = Injection

instance Category Injection where
  id = Injection id Just
  inj1 . inj2 =
    Injection (inject inj1 . inject inj2) (project inj2 <=< project inj1)

injectSubst :: Subst a a => Injection b a -> Name a -> b -> a -> a
injectSubst inj v x = subst v (inject inj x)

oneSubstInj ::
  Injection a b ->
  Name a -> a -> Substitution b
oneSubstInj inj x y = oneSubst (coerce x) (inject inj y)

lookupInj ::
  Injection a b ->
  Substitution b -> Name a -> Maybe a
lookupInj inj sbst x = project inj =<< Substitution.lookup (coerce x) sbst

extendInj ::
  Injection a b ->
  Substitution b -> Name a -> a -> Substitution b
extendInj inj sbst x = extend sbst (coerce x) . inject inj

