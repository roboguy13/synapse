import Data.Functor
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Const

-- #define SUBST_INSTANCES(C)\
--   instance (Subst C (f (Fix f))) => Subst C (Fix f);\
--   instance (Subst C (f a), Subst C (g a)) => Subst C (Product f g a);\
--   instance (Subst C (f (g a))) => Subst C (Compose f g a);\
--   instance (Subst C a) => Subst C (Const a b)

-- #define SUBST_INSTANCES1(C)\
--   instance (Subst (C a) b, Subst a b, Alpha b) => Subst (C a) (C b);\
--   instance () => Subst (C (Fix f)) b

  -- instance (Subst (C (Fix f)) (f (Fix f))) => Subst (C (Fix f)) (Fix f);\
  -- instance (Subst (C (f (Fix f))) b) => Subst (C (Fix f)) b;\
  -- instance (Subst (C (Product f g a)) (f a), Subst (C (Product f g a)) (g a)) => Subst (C (Product f g a)) (Product f g a);\
  -- instance (Subst (C (Compose f g a)) (f (g a))) => Subst (C (Compose f g a)) (Compose f g a);\
  -- instance (Subst (C (Const a b)) a) => Subst (C (Const a b)) (Const a b)

-- #define SUBST_INSTANCES1(C)\
  -- instance (Subst (C (Fix f)) (f (Fix f))) => Subst (C (Fix f)) (Fix f);\
  -- instance (Subst (C (Fix f)) (f (Fix f)), Subst (TermX (Fix f)) (g (h a))) => Subst (C (Fix f)) (Compose g h a);\
  -- instance (Subst (C (Fix f)) (f (Fix f)), Subst (TermX (Fix f)) (g a), Subst (TermX (Fix f)) (h a)) => Subst (C (Fix f)) (Product g h a);\
  -- instance (Subst (C (Fix f)) (f (Fix f)), Subst (TermX (Fix f)) a) => Subst (C (Fix f)) (Const a b);\
