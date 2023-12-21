{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Synapse.Syntax.Context where

import Synapse.Syntax.Judgment
import Synapse.Syntax.Term
import Synapse.Logic.Unify
import Synapse.Logic.Substitution
import Synapse.Ppr
import Synapse.Orphans

import GHC.Generics

import Data.Void

import Unbound.Generics.LocallyNameless

import Data.Functor.Compose
import Data.Fix

#include "src/Synapse/SubstUtils.hs"

-- TODO: Support concatenating contexts
data Context
  = Empty
  | CtxVar (Name Context)
  | Extend Context Judgment
  deriving (Show, Generic)

instance Ppr Context where
  ppr Empty = text "<>"
  ppr (CtxVar x) = text "?" <.> ppr x
  ppr (Extend ctx j) = ppr ctx <.> text "," <+> ppr j

data HypJudgment =
  HypJudgment
  { hypJudgmentCtx :: Context
  , hypJudgmentBody :: Judgment
  }
  deriving (Show, Generic)

data SomeJudgment
  = SomeBasicJudgment Judgment
  | SomeHypJudgment HypJudgment
  deriving (Show, Generic)

instance Ppr HypJudgment where
  ppr j = ppr (hypJudgmentCtx j) <+> text "|-" <+> ppr (hypJudgmentBody j)

instance Ppr SomeJudgment where
  ppr (SomeBasicJudgment x) = ppr x
  ppr (SomeHypJudgment y) = ppr y

instance Alpha Context

SUBST_INSTANCES(Context)

instance Subst Context Judgment
instance Subst Context a => Subst Context (Substitution a)
-- instance Subst (TermX x)

instance Subst Context JudgmentSpec
instance (Subst Context x, Alpha x) => Subst Context (TermX x)
instance Subst Context BinderSort
instance Subst Context a => Subst Context (SpecPart' a)
instance Subst Context TermSpec
instance Subst Context TermSpecAlt
instance Subst Context Void

instance Subst Context Context where
  isvar (CtxVar x) = Just $ SubstName x
  isvar _ = Nothing

instance Match Context where
  isConst Empty = True
  isConst _ = False

  mkVar = CtxVar

  isVar (CtxVar x) = Just x
  isVar _ = Nothing

  -- getChildren (Extend 

-- matchHypJudgment :: HypJudgment -> HypJudgment -> Maybe (Substitution Term)
-- matchHypJudgment matcher j = undefined

