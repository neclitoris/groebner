module Poly.Variables
  ( Vars
  , Subset
  , weaken
  , ifSubset
  ) where

import Data.Kind
import Data.Singletons
import Data.List
import Data.List.Singletons
import Data.Maybe
import GHC.TypeLits
import GHC.TypeLits.Singletons

import Prelude.Singletons


type Vars = [Symbol]

type family Subset (v1 :: Vars) (v2 :: Vars) :: Constraint where
  Subset '[]    _     = ()
  Subset (v:vs) (vs') = ((Elem v vs') ~ True, Subset vs vs')

weaken :: Subset v1 v2 => Sing v1 -> Sing v2 -> [Int]
weaken s1 s2 = let v1 = fromSing s1
                   v2 = fromSing s2
                in map (fromJust . (`elemIndex` v2)) v1

ifSubset :: forall v1 v2 r .
                Sing v1 ->
                Sing v2 ->
                (Subset v1 v2 => r) ->
                Maybe r
ifSubset v1 v2 r =
  case v1 of
    SNil -> Just r
    v `SCons` vs ->
      case v `sElem` v2 of
        STrue  -> ifSubset vs v2 r
        SFalse -> Nothing


