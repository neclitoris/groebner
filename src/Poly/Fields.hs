{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Poly.Fields
  ( -- * Rational numbers
    Q
  , (%%)

  -- * Finite fields
  , GF

  -- * Other
  , PrimeW
  , SomePrimeW(..)
  , pattern PrimeW
  , isPrime
  , primes
  ) where

import Control.Monad.Fix
import Control.Monad
import Data.Bifunctor
import Data.Bool.Singletons
import Data.Constraint
import Data.Kind
import Data.List.Ordered
import Data.Ord.Singletons (sCompare, SOrdering(..))
import Data.Ratio
import Data.Reflection
import Data.Singletons
import Data.Traversable
import GHC.Natural
import GHC.TypeLits
import GHC.TypeLits.Singletons

import Poly.Polynomial


newtype Q = Q Rational deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

(%%) :: PolynomialConstraint (Polynomial Q v o)
     => Integer -> Integer -> Polynomial Q v o
(%%) n = toPolynomial . Q . (%) n

instance Show Q where
  show (Q r) = show (numerator r) ++
    if denominator r == 1
      then []
      else "%%" ++ show (denominator r)


newtype GF (p :: Natural) = GF Natural deriving Eq

instance Show (GF p) where
  show (GF n) = show n

instance Num (GF p) => Read (GF p) where
  readsPrec d r = map (first fromInteger) $ readsPrec d r

instance (1 <= p, SingI p) => Num (GF p) where
  GF n + GF m = GF ((n + m) `mod` FromSing (sing :: Sing p))
  negate (GF n) = GF (FromSing (sing :: Sing p) - n)

  GF n * GF m = GF ((n * m) `mod` FromSing (sing :: Sing p))

  fromInteger n = let p = naturalToInteger $ FromSing (sing :: Sing p)
                   in GF (fromInteger $ n `mod` p)

  abs    = id
  signum = const $ GF 1

data PrimeW n = PrimeW_

data SomePrimeW = forall n . (1 <= n, KnownNat n) => SomePrimeW (PrimeW n)

instance Eq SomePrimeW where
  (==) = ((== EQ) . ) . compare

instance Ord SomePrimeW where
  compare (SomePrimeW (PrimeW_ @p1)) (SomePrimeW (PrimeW_ @p2)) =
    FromSing $ sCompare (sing :: Sing p1) (sing :: Sing p2)

pattern PrimeW :: PrimeW n
pattern PrimeW <- PrimeW_

instance (Num (GF p), SingI p, Given (PrimeW p)) => Fractional (GF p) where
  recip (GF n) = let GCDEx x _ _ = gcdex (naturalToInteger n) p
                     p = naturalToInteger $ FromSing (sing :: Sing p)
                  in fromInteger $ x `mod` p

  fromRational r = let x = numerator r
                       y = denominator r
                    in fromInteger x / fromInteger y

-- | Primality test of type level naturals is beyond me.
-- Use this to ensure primality.
isPrime :: Natural -> Maybe SomePrimeW
isPrime n =
  case dropWhile ((<n) . fst) $ zip primes primeWitnesses of
    (r, w):_ | r == n -> Just w
    _                 -> Nothing


primes :: [Natural]
primes = 2 : 3 : 5 : minus [6 * n + r | n <- [1..], r <- [1, 5]] composites
  where
    composites = mergeAll [[p^2, p^2 + 2*p..] | p <- primes]

primeWitnesses :: [SomePrimeW]
primeWitnesses = do
  n <- primes
  SomeSing (s :: Sing p) <- pure $ toSing n
  STrue <- pure $ (sing :: Sing 1) %<=? s
  withKnownNat s $ pure $ SomePrimeW $ PrimeW_ @p

data GCDEx =
  GCDEx { gcdLhsCoef :: Integer
        , gcdRhsCoef :: Integer
        , gcdValue :: Integer
        } deriving Show

gcdex :: Integer -> Integer -> GCDEx
gcdex a 0 = GCDEx 1 0 a
gcdex a b = let (GCDEx x y g) = gcdex b (a `rem` b)
             in GCDEx y (x - (a `quot` b) * y) g
