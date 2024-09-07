module Poly.Point
  ( Point(..)
  , EvaluateAt(..)
  , asVector
  ) where

import Data.Finite (Finite)
import Data.Kind
import Poly.Variables
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Sized qualified as VN

import GHC.TypeLits
import GHC.TypeLits.Singletons
import Prelude.Singletons

class Point t where
  type Dimension t :: Nat
  type Field     t :: Type
  at :: t -> Finite (Dimension t) -> Field t

instance VG.Vector vec f => Point (VN.Vector vec n f) where
  type Dimension (VN.Vector vec n f) = n
  type Field     (VN.Vector vec n f) = f
  at = VN.index

asVector :: (KnownNat (Dimension p), Point p, VG.Vector v (Field p)) => p -> v (Field p)
asVector p = VG.convert $ V.map (p `at`) $ V.fromList [0..]

infix 8 @.
class Point t => EvaluateAt p t where
  (@.) :: p -> t -> Field t

