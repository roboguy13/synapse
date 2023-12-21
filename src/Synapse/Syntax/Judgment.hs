module Synapse.Syntax.Judgment
  where

import Synapse.Syntax.Term
import Synapse.Logic.Substitution
import Synapse.Ppr

import Control.Monad

data SpecPart
  = ParamSpot
  | OperatorPart String
  deriving (Show)

data JudgmentSpec =
  JudgmentSpec
  { judgmentSpecParts :: [SpecPart]
  , judgmentSpecArity :: [TermSpec]
  }
  deriving (Show)

data Judgment =
  Judgment
  { judgmentSpec :: JudgmentSpec
  , judgmentSpots :: [Term]
  }
  deriving (Show)

isJudgmentWellFormed :: Judgment -> Maybe [Substitution Term]
isJudgmentWellFormed jd =
    map snd <$> zipWithM termMatchesSpec (judgmentSpots jd) (judgmentSpecArity (judgmentSpec jd))

instance Ppr JudgmentSpec where
  ppr = mconcat . map go . judgmentSpecParts
    where
      go ParamSpot = text "_"
      go (OperatorPart s) = text s

instance Ppr Judgment where
  ppr jd = go (judgmentSpecParts (judgmentSpec jd)) (judgmentSpots jd)
    where
      go [] [] = mempty
      go (ParamSpot : rest) (x:xs) = ppr x <+> go rest xs
      go (OperatorPart s : rest) xs = text s <+> go rest xs

