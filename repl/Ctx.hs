module Ctx where

import Data.Singletons
import Data.HashMap.Strict

import Poly.Algorithms
import Poly.Polynomial
import Poly.Monomial.Order


data WrappedPolynomial where
  WrappedPolynomial :: SingI v => Polynomial Double v Lex -> WrappedPolynomial

data WrappedOrder where
  WrappedOrder :: MonomialOrder o => o -> WrappedOrder

data Value = Poly WrappedPolynomial | Builtin
data RetValue = RPoly WrappedPolynomial | RList [WrappedPolynomial] | RBuiltin String

data Ctx = Ctx { ctxVars :: HashMap String Value
               , ctxOrder :: WrappedOrder }

defaultCtx = Ctx { ctxVars = fromList $ zip l (replicate (length l) Builtin)
                 , ctxOrder = WrappedOrder Lex
                 }
  where
    l = ["S", "GroebnerBasis", "Autoreduce"]

updateCtx :: HashMap String Value -> Ctx -> Ctx
updateCtx h (Ctx v o) = Ctx (union h v) o

updateCtxNoShadow :: HashMap String Value -> Ctx -> Ctx
updateCtxNoShadow h (Ctx v o) = Ctx (union v h) o
