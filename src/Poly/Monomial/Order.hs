{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Poly.Monomial.Order
  ( MonomialData(..)
  , MonomialOrder(..)
  , Ordered(..)

  , Lex(..)
  , RevLex(..)
  , Graded(..)
  ) where

import Data.Kind
import Data.Vector.Storable ((!))
import Data.Vector.Storable qualified as VS

import Poly.Monomial.Internal


class MonomialOrder order where
  order       :: order
  monoCompare :: order -> MonomialData f -> MonomialData f -> Ordering

class MonomialOrder (Order a) => Ordered a where
  type WithOrder a :: Type -> Type
  type Order     a :: Type

  withOrder :: MonomialOrder o => o -> a -> WithOrder a o


data Lex = Lex deriving Show

instance MonomialOrder Lex where
  order = Lex

  monoCompare _ (mdPowers -> l) (mdPowers -> r) = go 0
    where
      go i
        | i == VS.length l = EQ
        | otherwise = compare (l ! i) (r ! i) <> go (i + 1)


data RevLex = RevLex deriving Show

instance MonomialOrder RevLex where
  order = RevLex

  monoCompare _ (mdPowers -> l) (mdPowers -> r) = rev $ go (VS.length l - 1)
    where
      go i
        | i == 0 = EQ
        | otherwise = compare (l ! i) (r ! i) <> go (i - 1)
      rev LT = GT
      rev EQ = EQ
      rev GT = LT


newtype Graded order = Graded order deriving Show

instance MonomialOrder order => MonomialOrder (Graded order) where
  order = Graded order

  monoCompare (Graded order) l r =
    (sum l `compare` sum r) <> monoCompare order l r
      where
        sum = VS.sum . mdPowers
