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

  monoCompare _ l r = mdPowers l `compare` mdPowers r


data RevLex = RevLex deriving Show

instance MonomialOrder RevLex where
  order = RevLex

  monoCompare _ l r =
    unRev $
      VS.foldMap' (Reverse . toEnum) $
      VS.zipWith (\x y -> fromEnum $ compare x y) (mdPowers l) (mdPowers r)


newtype Graded order = Graded order deriving Show

instance MonomialOrder order => MonomialOrder (Graded order) where
  order = Graded order

  monoCompare (Graded order) l r =
    (sum l `compare` sum r) <> monoCompare order l r
      where
        sum = VS.sum . mdPowers


newtype RevOrdering =
  RevOrdering { unRev :: Ordering }
  deriving Show

pattern Reverse o <- RevOrdering o where
  Reverse LT = RevOrdering GT
  Reverse EQ = RevOrdering EQ
  Reverse GT = RevOrdering LT

instance Semigroup RevOrdering where
  (<>) = mappend

instance Monoid RevOrdering where
  mempty = Reverse EQ

  mappend l (Reverse EQ) = l
  mappend _ r            = r
