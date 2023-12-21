module Synapse.Syntax.Context where

import Synapse.Syntax.Judgment
import Synapse.Ppr

-- TODO: Support concatenating contexts
data Context
  = Empty
  | CtxVar String
  | Extend Context Judgment
  deriving (Show)

instance Ppr Context where
  ppr Empty = text "<>"
  ppr (CtxVar x) = text "?" <.> text x
  ppr (Extend ctx j) = ppr ctx <.> text "," <+> ppr j

data HypJudgment =
  HypJudgment
  { hypJudgmentCtx :: Context
  , hypJudgmentBody :: Judgment
  }
  deriving (Show)

data SomeJudgment
  = SomeBasicJudgment Judgment
  | SomeHypJudgment HypJudgment
  deriving (Show)

instance Ppr HypJudgment where
  ppr j = ppr (hypJudgmentCtx j) <+> text "|-" <+> ppr (hypJudgmentBody j)

instance Ppr SomeJudgment where
  ppr (SomeBasicJudgment x) = ppr x
  ppr (SomeHypJudgment y) = ppr y

