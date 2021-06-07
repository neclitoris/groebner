{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Poly.Polynomial
  ( Polynomial
  , IsPolynomial(..)
  , PolynomialConstraint

  -- * Monomial views
  , monomials
  , leading

  -- * Other
  , withVariables
  ) where

import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Singletons

import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP

import Poly.Monomial
import Poly.Monomial.Variables


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

instance PolynomialConstraint (Polynomial f v o)
    => IsPolynomial f v o f where
  toPolynomial 0 = 0
  toPolynomial n = Polynomial $ (:[]) $ constant n

instance PolynomialConstraint (Polynomial f v o)
    => IsPolynomial f v o (Monomial f v o) where
  toPolynomial (coef -> 0) = 0
  toPolynomial m           = Polynomial [m]

instance PolynomialConstraint (Polynomial f v o)
    => IsPolynomial f v o (Polynomial f v o) where
  toPolynomial = id


-- | Lift a list of variable names to type level, and pass list of
-- polynomials representing them to a continuation. This can be used
-- like:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > withVariables ["x", "y", "z"] $ \[x, y, z] ->
-- >   print (x * y + z^2)
withVariables :: (Fractional f, Eq f, Show f)
              => Demote Vars
              -> (forall v. SingI v => [Polynomial f v Lex] -> r)
              -> r
withVariables v f = withSomeSing v \(s :: Sing v) ->
  withSingI s $ f $ map (Polynomial . (:[])) (variables @v)

-- | View the leading monomial of a polynomial, if it's not zero.
leading :: Polynomial f v o -> Maybe (Monomial f v o)
leading = listToMaybe . monomials


instance PolynomialConstraint (Polynomial f v o)
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
instance PolynomialConstraint (Polynomial field vars order)
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

  (monomials -> l) * (monomials -> r) =
    sum . map (Polynomial . (:[])) $ mulM <$> l <*> r

  fromInteger 0 = Polynomial []
  fromInteger i = Polynomial [constant (fromInteger i)]

  negate = Polynomial . map (mulFM (-1)) . monomials

  abs = Polynomial . (\l -> map (mulFM (1 / coef (head l))) l) . monomials

  signum = maybe 0 (toPolynomial . coef) . leading

instance (Ordered (Monomial f v o), Eq f) => Ordered (Polynomial f v o) where
  type WithOrder (Polynomial f v o) = Polynomial f v
  type Order     (Polynomial f v o) = o

  withOrder o = Polynomial . L.sortBy (flip compare) . map (withOrder o) . monomials
