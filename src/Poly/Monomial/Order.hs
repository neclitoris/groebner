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
import Data.Vector.Unboxed ((!))
import Data.Vector.Unboxed qualified as V


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
  monoCompare      :: order -> V.Vector Int -> V.Vector Int -> Ordering

class MonomialOrder (Order a) => Ordered a where
  type WithOrder a :: Type -> Type
  type Order     a :: Type

  withOrder :: MonomialOrder o => o -> a -> WithOrder a o


-- | Lexicographic ordering.
data Lex = Lex deriving Show

instance MonomialOrder Lex where
  order = Lex

  monoCompare _ = compare


-- | Reverse lexicographic ordering. Strictly speaking, it's not
-- a valid monomial order, so it shouldn't be used directly, but
-- defining it like this is good for composability (i.e. defining
-- degrevlex).
data RevLex = RevLex deriving Show

instance MonomialOrder RevLex where
  order = RevLex

  monoCompare _ l r = compare EQ $ V.reverse l `compare` V.reverse r


-- | Graded ordering. First compares total degree of monomials, using
-- its parameter order as tie breaker in case of equality.
newtype Graded order = Graded order deriving Show

instance MonomialOrder order => MonomialOrder (Graded order) where
  order = Graded order

  monoCompare (Graded order) l r = (V.sum l `compare` V.sum r) <> monoCompare order l r


type DegLex = Graded Lex

-- | Degree lexicographic ordering.
pattern DegLex :: DegLex
pattern DegLex = Graded Lex

type DegRevLex = Graded RevLex

-- | Degree reverse lexicographic ordering.
pattern DegRevLex :: DegRevLex
pattern DegRevLex = Graded RevLex
