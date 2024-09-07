module Poly.Rational where

import Poly.Polynomial
import Poly.Point

import Data.List.Singletons
import Data.Singletons

data Ratio f v o = Ratio (Polynomial f v o) (Polynomial f v o)


instance (Fractional f, Point p, SingI v, Field p ~ f, Dimension p ~ Length v)
    => EvaluateAt (Ratio f v o) p where
  (Ratio f g) @. v = f @. v / g @. v
