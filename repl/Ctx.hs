module Ctx where

import Data.Singletons
import Data.Type.Equality
import Data.HashMap.Strict
import GHC.TypeLits

import Poly.Algorithms
import Poly.Fields
import Poly.Polynomial
import Poly.Monomial.Order


data FieldType n where
  FDouble :: FieldType Double
  FRational :: FieldType Rational
  FGF :: forall p . KnownNat p => FieldType (GF p)

instance TestEquality FieldType where
  testEquality FDouble FDouble = Just Refl
  testEquality FRational FRational = Just Refl
  testEquality (FGF @p) (FGF @p') = apply Refl <$> testEquality (SNat @p) (SNat @p')
  testEquality _ _ = Nothing

data SomeField = forall n . SomeField (FieldType n)

data Value f o where
  Value :: PolynomialConstraint (Polynomial f v o)
        => Polynomial f v o -> Value f o

instance MonomialOrder o => Ordered (Value f o) where
  type Order (Value f o) = o
  type WithOrder (Value f o) = Value f
  withOrder o (Value p) = Value (withOrder o p)

data Ctx where
  Ctx :: forall f o . (Fractional f, Eq f, Read f, Show f, MonomialOrder o)
      => FieldType f -> o -> HashMap String (Value f o) -> Ctx

defaultCtx = Ctx FDouble Lex empty

updateCtx :: MonomialOrder o
          => FieldType f -> HashMap String (Value f o) -> Ctx -> Ctx
updateCtx f h (Ctx f' o v) =
  case testEquality f f' of
    Just Refl -> Ctx f' o (fmap (withOrder o) h `union` v)
    Nothing   -> Ctx f' o v

updateCtxNoShadow :: MonomialOrder o
                  => FieldType f -> HashMap String (Value f o) -> Ctx -> Ctx
updateCtxNoShadow f h (Ctx f' o v) =
  case testEquality f f' of
    Just Refl -> Ctx f' o (v `union` fmap (withOrder o) h)
    Nothing   -> Ctx f' o v

switchOrder :: MonomialOrder o => o -> Ctx -> Ctx
switchOrder o (Ctx f _ v) = Ctx f o (fmap (withOrder o) v)

switchField :: forall f . (Fractional f, Eq f, Read f, Show f)
            => FieldType f -> Ctx -> Ctx
switchField f (Ctx _ o _) = Ctx f o empty
