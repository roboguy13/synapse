module Synapse.Logic.Search
  where

import Synapse.Logic.Derivation
import Synapse.Syntax.Rule
import Synapse.Syntax.Context
import Synapse.Syntax.Term
import Synapse.Syntax.Judgment
import Synapse.Logic.Substitution
import Synapse.Logic.Unify

data QueryResult =
  QueryResult
  { queryResultDerivation :: Derivation SomeJudgment
  , queryResultTermSubst :: Substitution Term
  , queryResultJudgmentSubst :: Substitution SomeJudgment
  }

query :: [Rule] -> Query -> QueryResult
query = undefined

