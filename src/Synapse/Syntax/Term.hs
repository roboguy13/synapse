--
-- Terms in the object language. These are represented as s-expressions.
--

module Synapse.Syntax.Term
  where

import Synapse.Ppr

import Unbound.Generics.LocallyNameless

data BinderSort = BinderSort Int ([TermName] -> Term -> Doc)

binderSortId :: BinderSort -> Int
binderSortId (BinderSort i _) = i

data BinderSpec =
  BinderSpec [TermName] Term

type TermName = Name Term

data Term
  = Symbol String
  | IntLit Int
  | Var TermName
  | App Term [Term]
  | Binder BinderSort (Bind [TermName] Term)

instance Ppr Term where
  ppr (Symbol sym) = text sym
  ppr (IntLit i) = ppr i
  ppr (Var x) = ppr x
  ppr (App f args) =
    parens (hsep (map ppr (f : args)))

  -- TODO: Write this case
  -- ppr (Binder (BinderSort pprBinder) params body) = pprBinder params body

