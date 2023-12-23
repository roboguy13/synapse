{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Synapse.Syntax.Judgment
  where

#include "src/Synapse/SubstUtils.hs"

import Synapse.Syntax.Term
import Synapse.Logic.Substitution
import Synapse.Logic.Unify
import Synapse.Logic.SubstMap
import Synapse.Ppr
import Synapse.Orphans
import Synapse.Utils

import Unbound.Generics.LocallyNameless

import GHC.Generics

import Control.Monad
import Data.Void
import Data.Fix
import Data.List
import Data.Maybe
import Data.Ord

import Control.Lens.Plated

data SpecPart' a
  = ParamSpot a
  | OperatorPart String
  deriving (Show, Generic, Eq)

type SpecPart = SpecPart' ()

data JudgmentSpec =
  JudgmentSpec
  { judgmentSpecParts :: [SpecPart]
  , judgmentSpecArity :: [TermSpec]
  }
  deriving (Show, Generic, Eq)

data Judgment =
  Judgment
  { judgmentSpec :: JudgmentSpec
  , judgmentSpots :: [SubstTerm]
  }
  deriving (Show, Generic, Eq)

type instance ContainedTypes JudgmentSpec = '[SpecPart, TermSpec]
type instance ContainedTypes Judgment = '[JudgmentSpec, SubstTerm]

instance Plated JudgmentSpec where plate _ = pure
instance Subst JudgmentSpec JudgmentSpec
instance Subst JudgmentSpec a => Subst JudgmentSpec (SpecPart' a)
instance Subst JudgmentSpec TermSpec
instance Subst JudgmentSpec TermSpecAlt
instance Subst JudgmentSpec Void
instance Subst JudgmentSpec BinderSort
instance (Alpha a, Subst JudgmentSpec a) => Subst JudgmentSpec (TermX a)

instance Match JudgmentSpec where
  isConst _ = False
  mkVar_maybe = Nothing
  isVar _ = Nothing

  matchConstructor js1 js2 = do
    guard (judgmentSpecParts js1 == judgmentSpecParts js2)
    specs <- zipWithMaybe (NodePair undefined) (judgmentSpecArity js1) (judgmentSpecArity js2)
    undefined

  applyMatchSubst = undefined

isOperatorPart :: SpecPart' a -> Maybe String
isOperatorPart (OperatorPart s) = Just s
isOperatorPart _ = Nothing

getOperatorParts :: [SpecPart' a] -> [String]
getOperatorParts = mapMaybe isOperatorPart

isParamSpot :: SpecPart' a -> Maybe a
isParamSpot (ParamSpot x) = Just x
isParamSpot _ = Nothing

getParamSpots :: [SpecPart' a] -> [a]
getParamSpots = mapMaybe isParamSpot

sortSpecs :: [JudgmentSpec] -> [JudgmentSpec]
sortSpecs = sortBy go
  where
    go x y =
      compare (Down (getOperatorParts (judgmentSpecParts x))) (Down (getOperatorParts (judgmentSpecParts y)))

instance Alpha a => Alpha (SpecPart' a)
instance Alpha JudgmentSpec

isJudgmentWellFormed :: Judgment -> Maybe [MatchSubst Term]
isJudgmentWellFormed jd =
    map snd <$> zipWithM termMatchesSpec (map convertSubstTerm (judgmentSpots jd)) (judgmentSpecArity (judgmentSpec jd))

mergeSpec :: JudgmentSpec -> [SpecPart' String]
mergeSpec (JudgmentSpec xs0 ys0) = go xs0 ys0
  where
    go (ParamSpot ():restParts) (TermSpec (name:_) _:restSpecs) = ParamSpot name : go restParts restSpecs
    go (OperatorPart op:restParts) specs = OperatorPart op : go restParts specs
    go [] [] = []

instance Ppr JudgmentSpec where
  ppr = hsep . map go . mergeSpec
    where
      go (ParamSpot t) = text ('?':t)
      go (OperatorPart s) = text s

instance Ppr Judgment where
  ppr jd = go (judgmentSpecParts (judgmentSpec jd)) (judgmentSpots jd)
    where
      go [] [] = mempty
      go (ParamSpot _ : rest) (x:xs) = ppr x <+> go rest xs
      go (OperatorPart s : rest) xs = text s <+> go rest xs

-- SUBST_INSTANCES(Judgment)

instance Alpha Judgment
instance Subst Judgment Judgment
instance (Subst Judgment a) => Subst Judgment (Substitution a)
-- instance Subst Judgment (f (Fix f)) => Subst Judgment (Fix f)
instance Subst Judgment JudgmentSpec
instance Subst Judgment TermSpec
instance Subst Judgment TermSpecAlt
instance (Subst Judgment x, Alpha x) => Subst Judgment (TermX x)
instance Subst Judgment BinderSort
instance Subst Judgment a => Subst Judgment (SpecPart' a)
instance Subst Judgment Void

instance Plated Judgment where
  plate _ = pure

instance Match Judgment where
  isConst = null . getParamSpots . judgmentSpecParts . judgmentSpec
  mkVar_maybe = Nothing
  isVar _ = Nothing

  matchConstructor x y = do
    spots <- zipWithMaybe (NodePair (PathSubstMap (There Here))) (judgmentSpots x) (judgmentSpots y)
    Just
      (NodePair (PathSubstMap undefined) (judgmentSpec x) (judgmentSpec y)
      :spots)
      

-- matchJudgment :: Judgment -> Judgment -> Maybe (Substitution Term)
-- matchJudgment matcher j
--   | not (judgmentSpec matcher `aeq` judgmentSpec j) = Nothing
--   | otherwise =
--       runFreshMT $ matchList (zip (map convertSubstTerm (judgmentSpots matcher)) (map convertSubstTerm (judgmentSpots j)))

