{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Poly.Monomial.Order
  ( MonomialOrder(..)
  , Ordered(..)

  , Lex(..)
  , RevLex(..)
  , Graded(..)
  , DegLex
  , pattern DegLex
  , DegRevLex
  , pattern DegRevLex
  ) where

import Data.Kind
import Data.Vector.Storable ((!))
import Data.Vector.Storable qualified as VS


-- | Monomial order is a relation which is expected to satisfy the following
-- properties:
--
-- prop> not (m <= n) || mp <= np
-- prop> m <= mp
-- where m, n, p are arbitrary monomials.
--
-- Note that it doesn't consider coefficients.
class MonomialOrder order where
  -- | Monomial order type is expected to be a singleton type.
  order            :: order
  -- | Compare monomials represented by vector of powers of variables in
  -- lexicographic order.
  monoCompare      :: order -> VS.Vector Int -> VS.Vector Int -> Ordering

class MonomialOrder (Order a) => Ordered a where
  type WithOrder a :: Type -> Type
  type Order     a :: Type

  withOrder :: MonomialOrder o => o -> a -> WithOrder a o


-- | Lexicographic ordering.
data Lex = Lex deriving Show

instance MonomialOrder Lex where
  order = Lex

  monoCompare _ l r = l `compare` r


-- | Reverse lexicographic ordering. Strictly speaking, it's not
-- a valid monomial order, so it shouldn't be used directly, but
-- defining it like this is good for composability (i.e. defining
-- degrevlex).
data RevLex = RevLex deriving Show

instance MonomialOrder RevLex where
  order = RevLex

  monoCompare _ l r = compare EQ $ VS.reverse l `compare` VS.reverse r


-- | Graded ordering. First compares total degree of monomials, using
-- its parameter order as tie breaker in case of equality.
newtype Graded order = Graded order deriving Show

instance MonomialOrder order => MonomialOrder (Graded order) where
  order = Graded order

  monoCompare (Graded order) l r = (VS.sum l `compare` VS.sum r) <> monoCompare order l r


-- | Degree lexicographic ordering.
type DegLex = Graded Lex
pattern DegLex = Graded Lex

-- | Degree reverse lexicographic ordering.
type DegRevLex = Graded RevLex
pattern DegRevLex = Graded RevLex
