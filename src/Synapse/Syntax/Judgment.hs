{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Synapse.Syntax.Judgment
  where

import Synapse.Syntax.Term
import Synapse.Logic.Substitution
import Synapse.Logic.Unify
import Synapse.Ppr

import Unbound.Generics.LocallyNameless

import GHC.Generics

import Control.Monad

data SpecPart' a
  = ParamSpot a
  | OperatorPart String
  deriving (Show, Generic)

type SpecPart = SpecPart' ()

data JudgmentSpec =
  JudgmentSpec
  { judgmentSpecParts :: [SpecPart]
  , judgmentSpecArity :: [TermSpec]
  }
  deriving (Show, Generic)

data Judgment =
  Judgment
  { judgmentSpec :: JudgmentSpec
  , judgmentSpots :: [Term]
  }
  deriving (Show, Generic)

instance Alpha a => Alpha (SpecPart' a)
instance Alpha JudgmentSpec

isJudgmentWellFormed :: Judgment -> Maybe [Substitution Term]
isJudgmentWellFormed jd =
    map snd <$> zipWithM termMatchesSpec (judgmentSpots jd) (judgmentSpecArity (judgmentSpec jd))

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

instance Alpha Judgment
instance Subst Judgment Judgment
instance Subst Judgment JudgmentSpec
instance Subst Judgment TermSpec
instance Subst Judgment TermSpecAlt
instance Subst Judgment Term
instance Subst Judgment BinderSort
instance Subst Judgment a => Subst Judgment (SpecPart' a)

matchJudgment :: Judgment -> Judgment -> Maybe (Substitution Term)
matchJudgment matcher j
  | not (judgmentSpec matcher `aeq` judgmentSpec j) = Nothing
  | otherwise =
      runFreshMT $ matchList (zip (judgmentSpots matcher) (judgmentSpots j))

