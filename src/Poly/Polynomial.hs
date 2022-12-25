{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Poly.Polynomial
  (
  -- * Handy reexports
    module Poly.Variables

  -- * Polynomial type
  , Polynomial
  , IsPolynomial(..)
  , PolynomialConstraint
  , monomials
  , leading

  -- * Other
  , variables
  , withVariables
  , mapField
  , lift
  ) where

import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Singletons
import GHC.TypeLits.Singletons
import Prelude.Singletons

import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP

import Poly.Monomial hiding ( variables )
import Poly.Monomial qualified as M ( variables )
import Poly.Point
import Poly.Variables


-- | Type that represents polynomials. Parametrized by field of coefficients,
-- list of variables and monomial ordering.
newtype Polynomial (field :: Type) (vars :: Vars) (order :: Type) =
  Polynomial { monomials :: [Monomial field vars order] -- ^ View list of monomials
                                                        -- in order.
             }
  deriving (Eq)

-- | Constraint that means that polynomial supports all basic operations, defined
-- for convenience.
type family PolynomialConstraint p :: Constraint where
  PolynomialConstraint (Polynomial f v o)
    = (Fractional f, Eq f, SingI v, MonomialOrder o, Show f)


-- | Class that allows different types to be converted to `Polynomial`.
class IsPolynomial f v o t where
  toPolynomial :: t -> Polynomial f v o

instance (Num f, Eq f, SingI v)
    => IsPolynomial f v o f where
  toPolynomial 0 = Polynomial []
  toPolynomial n = Polynomial $ (:[]) $ constant n

instance (Num f, Eq f, SingI v)
    => IsPolynomial f v o (Monomial f v o) where
  toPolynomial (coef -> 0) = Polynomial []
  toPolynomial m           = Polynomial [m]

instance IsPolynomial f v o (Polynomial f v o) where
  toPolynomial = id

-- | List of polynomials that represent individual variables, in lexicographic
-- order.
variables :: forall v f . (Num f, SingI v)
          => [Polynomial f v Lex]
variables = Polynomial . (:[]) <$> M.variables

-- | Lift a list of variable names to type level, and pass list of
-- polynomials representing them to a continuation. This can be used
-- like:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > withVariables ["x", "y", "z"] $ \[x, y, z] ->
-- >   print (x * y + z^2)
withVariables :: Num f
              => Demote Vars
              -> (forall v. SingI v => [Polynomial f v Lex] -> r)
              -> r
withVariables v f = withSomeSing v \(s :: Sing v) -> withSingI s $ f $ variables @v

-- | View the leading monomial of a polynomial, if it's not zero.
leading :: Polynomial f v o -> Maybe (Monomial f v o)
leading = listToMaybe . monomials

-- | Map polynomial coefficients to another field.
mapField :: (f -> f') -> Polynomial f v o -> Polynomial f' v o
mapField f (monomials -> m) =
  Polynomial $ map (\(Monomial c p) -> Monomial (f c) p) m

-- | Lift polynomial to accept polynomial arguments.
lift :: (Num f, Eq f, SingI v)
     => Polynomial f v o -> Polynomial (Polynomial f v o) v o
lift = mapField toPolynomial


instance (Num f, Point p, SingI v, Field p ~ f, Dimension p ~ Length v)
    => EvaluateAt (Polynomial f v o) p where
  (monomials -> p) @. v = sum $ map (@.v) p

instance (Eq f, Num f, Show f, SingI v)
    => PP.Pretty (Polynomial f v o) where
  pretty (monomials -> []) = PP.pretty "0"
  pretty (monomials -> (m:ms)) =
    PP.fillSep $
      PP.pretty (if s == LT then "-" else "") <> x
      : map (\(s, x) -> PP.pretty (if s == LT then "-" else "+") <+> x) xs
    where
      (s, x) = prettySign m
      xs     = map prettySign ms

instance PP.Pretty (Polynomial f v o) => Show (Polynomial f v o) where
  showsPrec _ =
    PP.renderShowS . PP.layoutSmart PP.defaultLayoutOptions . PP.pretty

-- | `abs` normalizes the polynomial, `signum` returns the leading
-- coefficient. This behavior satisfies `Num` laws.
instance (Num field, Eq field, SingI vars, MonomialOrder order)
    => Num (Polynomial field vars order) where
  (monomials -> l) + (monomials -> r) =
    Polynomial $ impl l r where
      impl l      []     = l
      impl []     r      = r
      impl (x:xs) (y:ys) =
        case x `compare` y of
          GT -> x : impl xs (y : ys)
          LT -> y : impl (x : xs) ys
          EQ -> case x `unsafeAddM` y of
                  (coef -> 0) -> impl xs ys
                  s           -> s : impl xs ys

  (monomials -> l) * (monomials -> r) = Polynomial
    . filter ((/= 0) . coef)
    . map (foldr1 unsafeAddM)
    . L.groupBy addable
    . L.sortBy (flip compare)
    $ mulM <$> l <*> r

  fromInteger 0 = Polynomial []
  fromInteger i = Polynomial [constant (fromInteger i)]

  negate = Polynomial . map (mulFM (-1)) . monomials

  abs = id

  signum _ = toPolynomial (1 :: field)

instance (Ordered (Monomial f v o), Eq f) => Ordered (Polynomial f v o) where
  type WithOrder (Polynomial f v o) = Polynomial f v
  type Order     (Polynomial f v o) = o

  withOrder o = Polynomial . L.sortBy (flip compare) . map (withOrder o) . monomials
