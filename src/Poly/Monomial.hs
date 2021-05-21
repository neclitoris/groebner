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
  ( Monomial
  , pattern Monomial
  , mulM
  , addM
  , unsafeAddM
  , mulFM
  , constant
  , coef
  , powers
  , LCM(..)
  , lcm
  , divide
  , prettySign
  , module Poly.Monomial.Order
  ) where

import Control.Monad

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
import Poly.Monomial.Variables

import Prelude hiding (lcm)


newtype Monomial (field :: Type) (vars :: Vars) (order :: Type) =
  MonomialImpl { mData :: MonomialData field }
  deriving (Eq)

{-# COMPLETE MonomialVec #-}
pattern MonomialVec :: f -> VS.Vector Int -> Monomial f v o
pattern MonomialVec coef powers  = MonomialImpl (MonomialData coef powers)


viewMono :: Monomial f v o -> (f, [Int])
viewMono (MonomialImpl (MonomialData coef powers)) = (coef, VS.toList powers)

{-# COMPLETE Monomial #-}
pattern Monomial :: f -> [Int] -> Monomial f v o
pattern Monomial{coef, powers} <- (viewMono -> (coef, powers))
  where
    Monomial coef powers = MonomialImpl (MonomialData coef (VS.fromList powers))


mulM :: Fractional f => Monomial f v o -> Monomial f v o -> Monomial f v o
mulM (MonomialVec c1 p1) (MonomialVec c2 p2) =
  MonomialVec (c1 * c2) (VS.zipWith (+) p1 p2)

addM :: Fractional f
     => Monomial f v o -> Monomial f v o -> Maybe (Monomial f v o)
addM (Monomial c1 p1) (Monomial c2 p2) = do
  guard $ p1 == p2
  pure $ Monomial (c1 + c2) p1

unsafeAddM :: Fractional f
           => Monomial f v o -> Monomial f v o -> Monomial f v o
unsafeAddM (Monomial c1 p) (Monomial c2 _) = Monomial (c1 + c2) p

constant :: forall f o v. (Fractional f, SingI v) => f -> Monomial f v o
constant f =
  MonomialVec f (VS.replicate l 0)
    where
      l = length $ fromSing (sing :: Sing v)

mulFM :: Fractional f => f -> Monomial f v o -> Monomial f v o
mulFM x (MonomialVec coef powers) = MonomialVec (x * coef) powers


data LCM f v o = LCM
  { lcmLhsMult :: Monomial f v o
  , lcmRhsMult :: Monomial f v o
  , lcmValue   :: Monomial f v o
  }
  deriving Show

lcm :: Fractional f => Monomial f v o -> Monomial f v o -> LCM f v o
lcm (MonomialVec c1 p1) (MonomialVec c2 p2) = LCM
  (MonomialVec c2 (VS.zipWith (-) res p1))
  (MonomialVec c1 (VS.zipWith (-) res p2))
  (MonomialVec (c1 * c2) res)
    where
      res = VS.zipWith max p1 p2

divide :: Fractional f
       => Monomial f v o -> Monomial f v o -> Maybe (Monomial f v o)
divide (MonomialVec c1 p1) (MonomialVec c2 p2) = do
  let pows = VS.zipWith (-) p1 p2
  guard (VS.all (>=0) pows)
  pure $ MonomialVec (c1 / c2) pows


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
  pretty m = let (sign, doc) = prettySign m
              in (if sign == LT then PP.pretty "-" else PP.emptyDoc) <> doc

instance PP.Pretty (Monomial f v o) => Show (Monomial f v o) where
  showsPrec _ =
    PP.renderShowS . PP.layoutSmart PP.defaultLayoutOptions . PP.pretty

instance (Eq f, MonomialOrder o) => Ord (Monomial f v o) where
  compare (MonomialImpl l) (MonomialImpl r) = monoCompare (order :: o) l r

instance MonomialOrder o => Ordered (Monomial f v o) where
  type WithOrder (Monomial f v o) = Monomial f v
  type Order     (Monomial f v o) = o

  withOrder _ m = MonomialImpl $ mData m
