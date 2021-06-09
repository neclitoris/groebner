{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Poly.Monomial
  (
  -- * Handy reexports
    module Poly.Monomial.Order

  -- * Monomial type
  , Monomial
  , coef
  , powers

  -- * Operations on monomials
  , mulM
  , addM
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
import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Vector.Storable qualified as VS
import Data.Singletons
#if MIN_VERSION_singletons(3,0,0)
import GHC.TypeLits.Singletons
#endif

import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP

import Poly.Monomial.Internal
import Poly.Monomial.Order
import Poly.Variables

import Prelude hiding (lcm)


-- | Type that represents monomials. Parametrized by field of coefficients,
-- list of variables and monomial ordering.
newtype Monomial (field :: Type) (vars :: Vars) (order :: Type) =
  MonomialImpl { mData :: MonomialData field }
  deriving (Eq)

{-# COMPLETE Monomial #-}
pattern Monomial :: f -> VS.Vector Int -> Monomial f v o
pattern Monomial coef powers  = MonomialImpl (MonomialData coef powers)

coef   (Monomial c _) = c
powers (Monomial _ p) = VS.toList p

-- | Multiply monomials.
mulM :: Fractional f => Monomial f v o -> Monomial f v o -> Monomial f v o
mulM (Monomial c1 p1) (Monomial c2 p2) =
  Monomial (c1 * c2) (VS.zipWith (+) p1 p2)

-- | Add monomials.
addM :: Fractional f
     => Monomial f v o -> Monomial f v o -> Maybe (Monomial f v o)
addM (Monomial c1 p1) (Monomial c2 p2) = do
  guard $ p1 == p2
  pure $ Monomial (c1 + c2) p1

-- | Add monomials, assuming they have equal terms.
unsafeAddM :: Fractional f
           => Monomial f v o -> Monomial f v o -> Monomial f v o
unsafeAddM (Monomial c1 p) (Monomial c2 _) = Monomial (c1 + c2) p

-- | Multiply monomial by a constant.
mulFM :: Fractional f => f -> Monomial f v o -> Monomial f v o
mulFM x (Monomial coef powers) = Monomial (x * coef) powers

-- | Constant monomial.
constant :: forall f o v. (Fractional f, SingI v) => f -> Monomial f v o
constant f = Monomial f (VS.replicate l 0)
    where
      l = length $ fromSing (sing :: Sing v)

-- | List of monomials that represent individual variables, in lexicographic
-- order.
variables :: forall v f . (Num f, SingI v) => [Monomial f v Lex]
variables = map (Monomial 1) pows
  where
    len  = length $ fromSing (sing @v)
    pows = map (\i -> VS.generate len (\j -> if i == j + 1 then 1 else 0)) [1..len]


data LCM f v o = LCM
  { lcmLhsMult :: Monomial f v o
  , lcmRhsMult :: Monomial f v o
  , lcmValue   :: Monomial f v o
  }
  deriving Show

-- | Compute least common multiple of two monomials.
lcm :: Fractional f => Monomial f v o -> Monomial f v o -> LCM f v o
lcm (Monomial c1 p1) (Monomial c2 p2) = LCM
  (Monomial c2 (VS.zipWith (-) res p1))
  (Monomial c1 (VS.zipWith (-) res p2))
  (Monomial (c1 * c2) res)
    where
      res = VS.zipWith max p1 p2

-- | Divide monomial by another, if possible.
divide :: Fractional f
       => Monomial f v o -> Monomial f v o -> Maybe (Monomial f v o)
divide (Monomial c1 p1) (Monomial c2 p2) = do
  let pows = VS.zipWith (-) p1 p2
  guard (VS.all (>=0) pows)
  pure $ Monomial (c1 / c2) pows


prettySign :: forall f v o ann . (SingI v, Show f, Fractional f, Eq f)
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

instance (SingI v, Show f, Fractional f, Eq f)
    => PP.Pretty (Monomial f v o) where
  pretty = uncurry (<>) . first (bool PP.emptyDoc (PP.pretty "-") . (==LT)) . prettySign

instance PP.Pretty (Monomial f v o) => Show (Monomial f v o) where
  showsPrec _ =
    PP.renderShowS . PP.layoutSmart PP.defaultLayoutOptions . PP.pretty

instance (Eq f, MonomialOrder o) => Ord (Monomial f v o) where
  compare (Monomial _ l) (Monomial _ r) = monoCompare (order :: o) l r

instance MonomialOrder o => Ordered (Monomial f v o) where
  type WithOrder (Monomial f v o) = Monomial f v
  type Order     (Monomial f v o) = o

  withOrder _ m = MonomialImpl $ mData m
