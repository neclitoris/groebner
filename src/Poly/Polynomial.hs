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
  , variables
  , withVariables
  , leading
  , IsPolynomial(..)
  , PolynomialConstraint(..)
  ) where

import Data.Kind
import Data.List qualified as L
import Data.Singletons

import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP

import Poly.Monomial
import Poly.Monomial.Variables


newtype Polynomial (field :: Type) (vars :: Vars) (order :: Type) =
  Polynomial { pData :: [Monomial field vars order] }
  deriving (Eq)

type family PolynomialConstraint p :: Constraint where
  PolynomialConstraint (Polynomial f v o)
    = (Fractional f, Eq f, SingI v, MonomialOrder o)


class IsPolynomial f v o t where
  toPolynomial :: t -> Polynomial f v o

instance PolynomialConstraint (Polynomial f v o)
    => IsPolynomial f v o f where
  toPolynomial = Polynomial . (:[]) . constant

instance PolynomialConstraint (Polynomial f v o)
    => IsPolynomial f v o (Monomial f v o) where
  toPolynomial = Polynomial . (:[])

instance PolynomialConstraint (Polynomial f v o)
    => IsPolynomial f v o (Polynomial f v o) where
  toPolynomial = id


variables :: forall v f . PolynomialConstraint (Polynomial f v Lex)
          => [Polynomial f v Lex]
variables = map (Polynomial . (:[]) . Monomial 1) pows
  where
    len  = length $ fromSing (sing @v)
    pows = map (\i -> map (\j -> if i == j then 1 else 0) [1..len]) [1..len]

withVariables :: forall f r . (Fractional f, Eq f)
              => Demote Vars
              -> (forall v. SingI v => [Polynomial f v Lex] -> r)
              -> r
withVariables v f =
  withSomeSing v \(s :: Sing v) -> withSingI s $ f $ variables @v

leading :: Polynomial f v o -> Maybe (Monomial f v o)
leading (pData -> (x:xs)) = Just x
leading _                 = Nothing


instance (PolynomialConstraint (Polynomial f v o), Show f)
    => PP.Pretty (Polynomial f v o) where
  pretty (pData -> []) = PP.pretty "0"
  pretty (map prettySign . pData -> ((s,x):xs)) =
    L.foldl'
      (\rem (s, x) -> rem <+> PP.pretty (if s == LT then "-" else "+") <+> x)
      ((if s == LT then PP.pretty "-" else PP.emptyDoc) <> x)
      xs

instance PP.Pretty (Polynomial f v o) => Show (Polynomial f v o) where
  showsPrec _ =
    PP.renderShowS . PP.layoutPretty PP.defaultLayoutOptions . PP.pretty

instance (SingI vars, Eq field, MonomialOrder order, Fractional field)
    => Num (Polynomial field vars order) where
  (pData -> l) + (pData -> r) =
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

  (pData -> l) * (pData -> r) =
    sum [ Polynomial $ filter ((/= 0) .coef) $ map (mulM y) l | y <- r ]

  fromInteger 0 = Polynomial []
  fromInteger i = Polynomial [constant (fromInteger i)]

  negate = Polynomial . map (mulFM (-1)) . pData

  -- These definitions just satisfy the laws of Num class, no
  -- other meaning.
  abs = id

  signum = const 1

instance (Ordered (Monomial f v o), Eq f) => Ordered (Polynomial f v o) where
  type WithOrder (Polynomial f v o) = Polynomial f v
  type Order     (Polynomial f v o) = o

  withOrder o = Polynomial . L.sortBy (flip compare) . map (withOrder o) . pData
