{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Poly.Fields
  ( Q
  , (%%)
  , GF
  , assertPrimality
  , module Data.Ratio
  ) where

import Control.Monad
import Data.Kind
import Data.Ratio
import Data.Singletons
import GHC.Natural
import GHC.TypeLits
import Language.Haskell.TH qualified as TH

import Poly.Polynomial


newtype Q = Q Rational deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

(%%) :: PolynomialConstraint (Polynomial Q v o)
     => Integer -> Integer -> Polynomial Q v o
(%%) n = toPolynomial . Q . (%) n

instance Show Q where
  show (Q r) = show (numerator r) ++ "%%" ++ show (denominator r)


newtype GF (p :: Nat) = GF Natural deriving Eq

instance Show (GF p) where
  show (GF n) = show n

instance (1 <= p, SingI p) => Num (GF p) where
  GF n + GF m = GF ((n + m) `mod` FromSing (sing :: Sing p))
  negate (GF n) = GF (FromSing (sing :: Sing p) - n)

  GF n * GF m = GF ((n * m) `mod` FromSing (sing :: Sing p))

  fromInteger n = let p = naturalToInteger $ FromSing (sing :: Sing p)
                   in GF (fromInteger $ n `mod` p)

  abs    = id
  signum = const $ GF 1

type family Prime (n :: Nat) :: Constraint

instance (Num (GF p), SingI p, Prime p) => Fractional (GF p) where
  recip (GF n) = let GCDEx x _ _ = gcdex (naturalToInteger n) p
                     p = naturalToInteger $ FromSing (sing :: Sing p)
                  in fromInteger $ x `mod` p

  fromRational r = let x = numerator r
                       y = denominator r
                    in fromInteger x / fromInteger y


-- Primality test of type level naturals is beyond me.
-- Use this to ensure primality.
assertPrimality :: Integer -> TH.Q [TH.Dec]
assertPrimality n =
  if not (isPrime n)
     then error $ show n ++ " is not prime"
     else [d| type instance Prime $(TH.litT $ TH.numTyLit n) = () |]
    where
      isPrime n = and [ n `mod` d /= 0 | d <- takeWhile ((<=n) . (^2)) [2..]]

data GCDEx =
  GCDEx { gcdLhsCoef :: Integer
        , gcdRhsCoef :: Integer
        , gcdValue :: Integer
        } deriving Show

gcdex :: Integer -> Integer -> GCDEx
gcdex a 0 = GCDEx 1 0 a
gcdex a b = let (GCDEx x y g) = gcdex b (a `rem` b)
             in GCDEx y (x - (a `quot` b) * y) g
