{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Poly.Fields
  ( Q
  , (%%)
  , GF
  , Prime
  , module Data.Ratio
  ) where

import Data.Kind
import Data.Ratio
import Data.Singletons
import GHC.Natural
import GHC.TypeLits

import Poly.Polynomial


type Q = Rational

(%%) :: PolynomialConstraint (Polynomial Q v o) => Integer -> Integer -> Polynomial Q v o
(%%) n = toPolynomial . (%) n

type family Prime (n :: Nat) :: Constraint

-- Primality test of type level naturals is beyond me.
-- Declare instances like this, if you need them for other
-- primes.
type instance Prime 2 = ()
type instance Prime 3 = ()
type instance Prime 5 = ()


newtype GF (p :: Nat) = GF Natural deriving (Show, Eq)


instance (1 <= p, SingI p) => Num (GF p) where
  GF n + GF m = GF ((n + m) `mod` FromSing (sing :: Sing p))
  negate (GF n) = GF (FromSing (sing :: Sing p) - n)

  GF n * GF m = GF ((n * m) `mod` FromSing (sing :: Sing p))

  fromInteger n = let p = naturalToInteger $ FromSing (sing :: Sing p)
                   in GF (fromInteger $ n `mod` p)

  abs    = id
  signum = const $ GF 1

instance (Num (GF p), SingI p, Prime p) => Fractional (GF p) where
  recip (GF n) = let GCDEx x _ _ = gcdex (naturalToInteger n) p
                     p = naturalToInteger $ FromSing (sing :: Sing p)
                  in fromInteger $ x `mod` p

  fromRational r = let x = numerator r
                       y = denominator r
                    in fromInteger x / fromInteger y


data GCDEx =
  GCDEx { gcdLhsCoef :: Integer
        , gcdRhsCoef :: Integer
        , gcdValue :: Integer
        } deriving Show

gcdex :: Integer -> Integer -> GCDEx
gcdex a 0 = GCDEx 1 0 a
gcdex a b = let (GCDEx x y g) = gcdex b (a `rem` b)
             in GCDEx y (x - (a `quot` b) * y) g
