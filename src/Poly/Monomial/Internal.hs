{-# LANGUAGE GADTs #-}
module Poly.Monomial.Internal
  ( MonomialData(..)
  ) where

import Data.Vector.Storable qualified as VS


data MonomialData field = MonomialData
  { mdCoef :: !field
  , mdPowers :: !(VS.Vector Int)
  } deriving (Show, Eq)

