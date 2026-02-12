module Ctx where

import Data.Singletons
import Data.Typeable
import Data.HashMap.Strict

import Poly.Algorithms
import Poly.Polynomial
import Poly.Monomial.Order


data Value f o where
  Value :: PolynomialConstraint (Polynomial f v o)
        => Polynomial f v o -> Value f o

instance MonomialOrder o => Ordered (Value f o) where
  type Order (Value f o) = o
  type WithOrder (Value f o) = Value f
  withOrder o (Value p) = Value (withOrder o p)

data Ctx where
  Ctx :: forall f o . (Fractional f, Eq f, Read f, Show f, Typeable f, MonomialOrder o)
      => o -> HashMap String (Value f o) -> Ctx

defaultCtx = Ctx @Double Lex empty

updateCtx :: forall f o . (Typeable f, MonomialOrder o)
          => HashMap String (Value f o) -> Ctx -> Ctx
updateCtx h (Ctx @f' @o' o v) =
  case eqT @f @f' of
    Just Refl -> Ctx o (fmap (withOrder o) h `union` v)
    Nothing   -> Ctx o v

updateCtxNoShadow :: forall f o . (Typeable f, MonomialOrder o)
                  => HashMap String (Value f o) -> Ctx -> Ctx
updateCtxNoShadow h (Ctx @f' @o' o v) =
  case eqT @f @f' of
    Just Refl -> Ctx o (v `union` fmap (withOrder o) h)
    Nothing   -> Ctx o v

switchOrder :: MonomialOrder o => o -> Ctx -> Ctx
switchOrder o (Ctx _ v) = Ctx o (fmap (withOrder o) v)

switchField :: forall f . (Fractional f, Eq f, Read f, Show f, Typeable f)
            => Proxy f -> Ctx -> Ctx
switchField p (Ctx o _) = Ctx @f o empty
