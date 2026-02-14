module FieldType where

import Data.Type.Equality
import GHC.TypeLits

import Poly.Fields

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
