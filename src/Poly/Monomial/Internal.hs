{-# LANGUAGE GADTs #-}
module Poly.Monomial.Internal
  ( MonomialData(..)
  ) where

import Data.Vector.Unboxed qualified as V


data MonomialData field = MonomialData
  { mdCoef :: !field
  , mdPowers :: !(V.Vector Int)
  } deriving (Show, Eq)

