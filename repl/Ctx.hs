module Ctx where

import Data.Singletons
import Data.HashMap.Strict

import Poly.Algorithms
import Poly.Polynomial
import Poly.Monomial.Order


data Value o where
  Value :: PolynomialConstraint (Polynomial Double v o)
        => Polynomial Double v o -> Value o

instance MonomialOrder o => Ordered (Value o) where
  type Order (Value o) = o
  type WithOrder (Value o) = Value
  withOrder o (Value p) = Value (withOrder o p)

data Ctx where
  Ctx :: forall o . MonomialOrder o => o -> HashMap String (Value o) -> Ctx

defaultCtx = Ctx Lex empty

updateCtx :: MonomialOrder o => HashMap String (Value o) -> Ctx -> Ctx
updateCtx h (Ctx o v) = Ctx o (fmap (withOrder o) h `union` v)

updateCtxNoShadow :: MonomialOrder o => HashMap String (Value o) -> Ctx -> Ctx
updateCtxNoShadow h (Ctx o v) = Ctx o (v `union` fmap (withOrder o) h)

switchOrder :: MonomialOrder o => o -> Ctx -> Ctx
switchOrder o (Ctx _ v) = Ctx o (fmap (withOrder o) v)
