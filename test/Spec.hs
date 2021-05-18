{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Monad.IO.Class
import Data.Ratio
import Data.List
import Data.Singletons
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality
import GHC.Natural

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty

import Poly.Algorithms
import Poly.Fields
import Poly.Monomial
import Poly.Monomial.Order
import Poly.Monomial.Variables
import Poly.Polynomial


tests :: Tasty.TestTree
tests = Tasty.testGroup "tests"
  [ properties
  ]

properties :: Tasty.TestTree
properties = Tasty.testGroup "properties"
  [ Tasty.testProperty "buchberger criterion" buchbergerCriterion
  ]


buchbergerCriterion :: Property
buchbergerCriterion = withTests 1000 $ property do
  numNames <- forAll $ Gen.int (Range.linear 2 4)
  let names = map (\n -> "x" <> Text.pack (show n)) [1..numNames]

  withVariables names \(vs :: [Polynomial (GF 5) v Lex]) -> do
    polys <- forAll $ filter (/=0) <$> Gen.list (Range.exponential 2 5) (genPoly genGF vs)

    let basis   = groebnerBasis $ map (withOrder (Graded RevLex)) polys
        sPolys  = [ s | (f:gs) <- tails basis, g <- gs
                      , let s = sPolynomial f g, s /= 0]
        reduced = map (reduceBySet basis) sPolys
    assert $ all (==0) reduced


genName :: MonadGen m => m Text
genName = Gen.text (Range.constant 1 1) Gen.lower

genQ :: MonadGen m => m Q
genQ = Gen.realFrac_ (Range.linearFrac (-10) 10)

genGF :: forall p m . (MonadGen m, SingI p, Fractional (GF p)) => m (GF p)
genGF = fromInteger <$> Gen.integral (Range.linear 1 p)
  where
    p = naturalToInteger $ FromSing (sing :: Sing p)

genPoly :: forall v f m . (PolynomialConstraint (Polynomial f v Lex), MonadGen m)
        => m f
        -> [Polynomial f v Lex]
        -> m (Polynomial f v Lex)
genPoly mCoef vars = do
  monos <- Gen.list (Range.exponential 0 4) (genMono mCoef vars)
  pure $ sum monos

genMono :: forall v f m . (PolynomialConstraint (Polynomial f v Lex), MonadGen m)
        => m f
        -> [Polynomial f v Lex]
        -> m (Polynomial f v Lex)
genMono mCoef vars = do
  coef   <- mCoef
  powers <- Gen.filterT ((< numVars @v) . sum)
              $ replicateM (numVars @v)
              $ Gen.int (Range.exponential 0 (numVars @v - 1))
  case coef of
    0 -> pure 0
    _ -> pure $ toPolynomial coef * product (zipWith (^) vars powers)

numVars :: forall (v :: Vars). SingI v => Int
numVars = length $ fromSing (sing :: Sing v)


main :: IO ()
main = Tasty.defaultMain tests
