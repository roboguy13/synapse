import Data.Functor
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Const

#define SUBST_INSTANCES(C)\
  instance (Subst C (f (Fix f))) => Subst C (Fix f);\
  instance (Subst C (f a), Subst C (g a)) => Subst C (Product f g a);\
  instance (Subst C (f (g a))) => Subst C (Compose f g a);\
  instance (Subst C a) => Subst C (Const a b)

