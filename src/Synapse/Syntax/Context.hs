{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Synapse.Syntax.Context where

import Prelude hiding (id, (.))

import Synapse.Syntax.Judgment
import Synapse.Syntax.Term
import Synapse.Logic.Unify
import Synapse.Logic.Substitution
import Synapse.Logic.Injection
import Synapse.Logic.SubstMap
import Synapse.Ppr
import Synapse.Orphans

import Control.Category

import GHC.Generics (Generic)

import Data.Void
import Data.Typeable

import Unbound.Generics.LocallyNameless

import Control.Lens.Plated
import Control.Lens.TH
import Control.Lens hiding (Context, Empty, (<.>))

import Data.Fix

#include "src/Synapse/SubstUtils.hs"

-- TODO: Support concatenating contexts
data Context
  = Empty
  | CtxVar (Name Context)
  | Extend Context Judgment
  deriving (Show, Generic, Eq)

instance Ppr Context where
  ppr Empty = text "<>"
  ppr (CtxVar x) = text "?" <.> ppr x
  ppr (Extend ctx j) = ppr ctx <.> text "," <+> ppr j

data HypJudgment =
  HypJudgment
  { _hypJudgmentCtx :: Context
  , _hypJudgmentBody :: Judgment
  }
  deriving (Show, Generic, Eq)

data SomeJudgment
  = SomeBasicJudgment Judgment
  | SomeHypJudgment HypJudgment
  deriving (Show, Generic, Eq)

makePrisms ''SomeJudgment
makeLenses ''HypJudgment

instance RenameTerms Context where
  renameTerms _ Empty = Empty
  renameTerms _ (CtxVar x) = CtxVar x
  renameTerms xs (Extend ctx j) =
    Extend (renameTerms xs ctx) (renameTerms xs j)

  termFVs Empty = []
  termFVs (CtxVar _) = []
  termFVs (Extend ctx j) =
    termFVs j ++ termFVs ctx

instance RenameTerms HypJudgment where
  renameTerms xs (HypJudgment ctx j) =
    HypJudgment (renameTerms xs ctx) (renameTerms xs j)

  termFVs (HypJudgment ctx j) =
    termFVs ctx ++ termFVs j

instance RenameTerms SomeJudgment where
  renameTerms xs (SomeBasicJudgment j) =
    SomeBasicJudgment (renameTerms xs j)

  renameTerms xs (SomeHypJudgment j) =
    SomeHypJudgment (renameTerms xs j)

  termFVs (SomeBasicJudgment j) = termFVs j
  termFVs (SomeHypJudgment j) = termFVs j

instance Alpha HypJudgment

-- SUBST_INSTANCES(HypJudgment)

instance Plated HypJudgment where
  plate _ = pure

instance Plated Context where
  plate _ Empty = pure Empty
  plate _ (CtxVar v) = pure (CtxVar v)
  plate f (Extend ctx j) = Extend <$> f ctx <*> pure j

instance Match Context where
  isConst Empty = True
  isConst _ = False

  isVar (CtxVar v) = Just v
  isVar _ = Nothing

  mkVar_maybe = Just CtxVar

  matchConstructor Empty Empty = Just []
  matchConstructor (Extend ctx j) (Extend ctx' j') =
    Just
      [NodePair ctx ctx'
      ,NodePair j j'
      ]

  applySubstMap sbst Empty = Empty
  applySubstMap sbst ctx@(CtxVar _) = applySubstitution (sbst ^. substLens) ctx
  applySubstMap sbst (Extend ctx j) =
    Extend (applySubstMap sbst ctx)
           (applySubstMap sbst j)

instance Match HypJudgment where
  isConst _ = False -- TODO: Is this right?
  mkVar_maybe = Nothing
  isVar _ = Nothing

  matchConstructor j1 j2 =
      Just
        [NodePair (_hypJudgmentCtx j1) (_hypJudgmentCtx j2)
        ,NodePair (_hypJudgmentBody j1) (_hypJudgmentBody j2)
        ]

  applySubstMap sbst (HypJudgment ctx body) =
    HypJudgment
      (applySubstMap sbst ctx)
      (applySubstMap sbst body)

instance Subst SomeJudgment SomeJudgment
instance Subst SomeJudgment Judgment
instance Subst SomeJudgment HypJudgment
instance Subst SomeJudgment Context
instance (Typeable a, Subst SomeJudgment a, Alpha a) => Subst SomeJudgment (TermX a)
instance Subst SomeJudgment BinderSort
instance Subst SomeJudgment JudgmentSpec
instance Subst SomeJudgment TermSpec
instance Subst SomeJudgment TermSpecAlt
instance Subst SomeJudgment Void
instance Subst SomeJudgment a => Subst SomeJudgment (SpecPart' a)
instance (Subst SomeJudgment a, Alpha a) => Subst SomeJudgment (Substitution a)

instance Alpha SomeJudgment

instance Plated SomeJudgment where
  plate _ = pure

instance Match SomeJudgment where
  isConst (SomeBasicJudgment j) = isConst j
  isConst (SomeHypJudgment j) = isConst j

  mkVar_maybe = Nothing
  isVar _ = Nothing

  matchConstructor (SomeBasicJudgment j1) (SomeBasicJudgment j2) =
    Just [NodePair j1 j2]
  matchConstructor (SomeHypJudgment j1) (SomeHypJudgment j2) =
    Just [NodePair j1 j2]
  matchConstructor _ _ = Nothing

  applySubstMap sbst (SomeBasicJudgment j) = SomeBasicJudgment $ applySubstMap sbst j
  applySubstMap sbst (SomeHypJudgment j) = SomeHypJudgment $ applySubstMap sbst j


instance Ppr HypJudgment where
  ppr j = ppr (_hypJudgmentCtx j) <+> text "|-" <+> ppr (_hypJudgmentBody j)

instance Ppr SomeJudgment where
  ppr (SomeBasicJudgment x) = ppr x
  ppr (SomeHypJudgment y) = ppr y

instance Alpha Context

instance Subst Context Context where
  isvar (CtxVar x) = Just $ SubstName x
  isvar _ = Nothing

-- SUBST_INSTANCES(Context)

instance Subst Context Judgment
instance Subst Context a => Subst Context (Substitution a)
-- instance Subst (TermX x)

instance Subst Context JudgmentSpec
instance (Typeable x, Subst Context x, Alpha x) => Subst Context (TermX x)
instance Subst Context BinderSort
instance Subst Context a => Subst Context (SpecPart' a)
instance Subst Context TermSpec
instance Subst Context TermSpecAlt
instance Subst Context Void

instance Subst HypJudgment Context
instance Subst HypJudgment HypJudgment
instance Subst HypJudgment Judgment
instance Subst HypJudgment JudgmentSpec
instance Subst HypJudgment TermSpec
instance Subst HypJudgment TermSpecAlt
instance (Subst HypJudgment a) => Subst HypJudgment (SpecPart' a)
instance Subst HypJudgment Void
instance (Subst HypJudgment a) => Subst HypJudgment (Substitution a)
instance (Typeable a, Alpha a, Subst HypJudgment a) => Subst HypJudgment (TermX a)
instance Subst HypJudgment BinderSort

-- matchHypJudgment :: HypJudgment -> HypJudgment -> Maybe (Substitution Term)
-- matchHypJudgment matcher j = undefined

