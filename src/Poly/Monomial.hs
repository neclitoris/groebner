{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Poly.Monomial
  (
  -- * Handy reexports
    module Poly.Monomial.Order

  -- * Monomial type
  , Monomial(..)
  , pattern Monomial
  , powers

  -- * Operations on monomials
  , weaken
  , mulM
  , addM
  , addable
  , unsafeAddM
  , mulFM
  , LCM(..)
  , lcm
  , divide

  -- * Other
  , constant
  , variables
  , prettySign
  ) where

import Control.Monad

import Data.Bool
import Data.Bifunctor
import Data.Function
import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Vector qualified as VV
import Data.Vector.Unboxed qualified as V
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Sized qualified as VN
import Data.Singletons
import GHC.TypeLits.Singletons
import Prelude.Singletons

import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP

import Poly.Monomial.Order
import Poly.Point
import Poly.Variables hiding ( weaken, common )
import Poly.Variables qualified as PV

import Prelude hiding (lcm)


-- | Type that represents monomials. Parametrized by field of coefficients,
-- list of variables and monomial ordering.
-- Use `MonomialImpl` constructor if you don't mind working with vectors
-- directly to get faster performance.
data Monomial (field :: Type) (vars :: Vars) (order :: Type) =
  MonomialImpl
  { coef :: !field
  , vPowers :: !(V.Vector Int)
  }
  deriving (Eq)

powers (MonomialImpl _ p) = V.toList p

-- | Use this pattern if you're lazy and want to work with lists instead
-- of vectors.
{-# COMPLETE Monomial #-}
pattern Monomial :: f -> [Int] -> Monomial f v o
pattern Monomial coef powers <- MonomialImpl coef (V.toList -> powers)
  where Monomial coef powers = MonomialImpl coef (V.fromList powers)

weaken :: forall v2 v1 f o . (Subset v1 v2, SingI v1, SingI v2)
       => Monomial f v1 o
       -> Monomial f v2 o
weaken (MonomialImpl c p) = MonomialImpl c (V.update zeros upds)
  where
    zeros = V.replicate (length (demote @v2)) 0
    upds  = V.zip (V.fromList $ PV.weaken (sing @v1) (sing @v2)) p

-- | Multiply monomials.
mulM :: Num f => Monomial f v o -> Monomial f v o -> Monomial f v o
mulM (MonomialImpl c1 p1) (MonomialImpl c2 p2) =
  MonomialImpl (c1 * c2) (V.zipWith (+) p1 p2)

-- | Add monomials.
addM :: Num f
     => Monomial f v o -> Monomial f v o -> Maybe (Monomial f v o)
addM (MonomialImpl c1 p1) (MonomialImpl c2 p2) = do
  guard $ p1 == p2
  pure $ MonomialImpl (c1 + c2) p1

addable :: Monomial f v o -> Monomial f v o -> Bool
addable = (==) `on` vPowers

-- | Add monomials, assuming they have equal terms.
unsafeAddM :: Num f
           => Monomial f v o -> Monomial f v o -> Monomial f v o
unsafeAddM (MonomialImpl c1 p) (MonomialImpl c2 _) = MonomialImpl (c1 + c2) p

-- | Multiply monomial by a constant.
mulFM :: Num f => f -> Monomial f v o -> Monomial f v o
mulFM x (MonomialImpl coef powers) = MonomialImpl (x * coef) powers

-- | Constant monomial.
constant :: forall f o v. SingI v => f -> Monomial f v o
constant f = MonomialImpl f (V.replicate l 0)
    where
      l = length (demote @v)

-- | List of monomials that represent individual variables, in lexicographic
-- order.
variables :: forall v f o . (Num f, SingI v) => [Monomial f v o]
variables = map (MonomialImpl 1) pows
  where
    len  = length $ fromSing (sing @v)
    pows = map (\i -> V.generate len (\j -> if i == j + 1 then 1 else 0)) [1..len]

instance (Num f, Point p, SingI v, Field p ~ f, Dimension p ~ Length v)
    => EvaluateAt (Monomial f v o) p where
  MonomialImpl c p @. v =
    withKnownNat (sLength (sing @v)) $
      c * VV.product (VV.zipWith (^) (asVector v) (GV.convert p))

data LCM f v o = LCM
  { lcmLhsMult :: Monomial f v o
  , lcmRhsMult :: Monomial f v o
  , lcmValue   :: Monomial f v o
  }
  deriving Show

-- | Compute least common multiple of two monomials.
lcm :: Num f => Monomial f v o -> Monomial f v o -> LCM f v o
lcm (MonomialImpl c1 p1) (MonomialImpl c2 p2) = LCM
  (MonomialImpl c2 (V.zipWith (-) res p1))
  (MonomialImpl c1 (V.zipWith (-) res p2))
  (MonomialImpl (c1 * c2) res)
    where
      res = V.zipWith max p1 p2

-- | Divide monomial by another, if possible.
divide :: Fractional f
       => Monomial f v o -> Monomial f v o -> Maybe (Monomial f v o)
divide (MonomialImpl c1 p1) (MonomialImpl c2 p2) = do
  guard (V.all id $ V.zipWith (>=) p1 p2)
  let pows = V.zipWith (-) p1 p2
  pure $ MonomialImpl (c1 / c2) pows


prettySign :: forall f v o ann . (SingI v, Show f, Num f, Eq f)
           => Monomial f v o -> (Ordering, PP.Doc ann)
prettySign m =
  (sign, c <> L.foldl' (<>) PP.emptyDoc (L.intersperse (PP.pretty "*") vars))
    where
      vars = catMaybes $ zipWith zipF varNames $ powers m

      zipF _ 0 = Nothing
      zipF x 1 = Just $ PP.pretty x
      zipF x p = Just $ mconcat [PP.pretty x, PP.pretty "^", PP.pretty p]

      varNames = fromSing (sing :: Sing v)

      sign = if signum (coef m) == -1 then LT else GT

      prettyAbs = PP.pretty . show . abs

      c = case (abs $ coef m, all (== 0) $ powers m) of
            (x, True) -> prettyAbs x
            (1, _)    -> PP.emptyDoc
            (x, _)    -> prettyAbs x <> PP.pretty "*"

instance (SingI v, Show f, Num f, Eq f)
    => PP.Pretty (Monomial f v o) where
  pretty = uncurry (<>) . first (bool PP.emptyDoc (PP.pretty "-") . (==LT)) . prettySign

instance PP.Pretty (Monomial f v o) => Show (Monomial f v o) where
  showsPrec _ =
    PP.renderShowS . PP.layoutSmart PP.defaultLayoutOptions . PP.pretty

instance (Eq f, MonomialOrder o) => Ord (Monomial f v o) where
  compare (MonomialImpl _ l) (MonomialImpl _ r) = monoCompare (order :: o) l r

instance MonomialOrder o => Ordered (Monomial f v o) where
  type WithOrder (Monomial f v o) = Monomial f v
  type Order     (Monomial f v o) = o

  withOrder _ (MonomialImpl c p) = MonomialImpl c p
