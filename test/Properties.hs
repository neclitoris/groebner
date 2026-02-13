{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Properties
  ( properties
  ) where
import Control.Monad
import Data.List
import Data.Maybe
import Data.Reflection
import Data.Singletons
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Natural

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty

import Poly.Algorithms hiding (gcd)
import Poly.Algorithms qualified as Algo
import Poly.Fields
import Poly.Monomial
import Poly.Monomial.Order
import Poly.Polynomial

import Util


properties :: Tasty.TestTree
properties = Tasty.testGroup "properties"
  [ Tasty.testGroup "buchberger"
    [ Tasty.testProperty "buchberger criterion" buchbergerCriterion
    , Tasty.testProperty "contains original ideal" containsOriginal
    , Tasty.testProperty "order invariance" orderInvariance
    ]
  , Tasty.testGroup "GCD"
    [ Tasty.testProperty "GCD reflexivity" gcdRefl
    , Tasty.testProperty "GCD is indeed GCD" gcdisgcd
    ]
  ]


buchbergerCriterion :: Property
buchbergerCriterion = withTests 1000 $ property do
  numNames <- forAll $ Gen.int (Range.linear 2 4)
  let names = map (\n -> "x" <> Text.pack (show n)) [1..numNames]
  Just (SomePrimeW w@(PrimeW @p)) <- pure $ isPrime 5

  give w $ withVariables names \(vs :: [Polynomial (GF p) v Lex]) -> do
    polys <- forAll $ filter (/=0) <$> Gen.list (Range.exponential 2 5) (genPoly genGF vs)

    let basis   = groebnerBasis $ map (withOrder DegRevLex) polys
        sPolys  = [ s | (f:gs) <- tails basis, g <- gs
                      , let s = sPolynomial f g, s /= 0]
        reduced = map (leadReduceBySet basis) sPolys

    annotateShow basis
    annotateShow reduced

    assert $ all (==0) reduced

containsOriginal :: Property
containsOriginal = property do
  numNames <- forAll $ Gen.int (Range.linear 2 4)
  let names = map (\n -> "x" <> Text.pack (show n)) [1..numNames]
  Just (SomePrimeW w@(PrimeW @p)) <- pure $ isPrime 5

  give w $ withVariables names \(vs :: [Polynomial (GF p) v Lex]) -> do
    polys <- forAll $ filter (/=0) <$> Gen.list (Range.exponential 2 5) (genPoly genGF vs)

    let basis   = groebnerBasis polys
        reduced = map (leadReduceBySet basis) polys

    annotateShow basis
    annotateShow reduced

    assert $ all (==0) reduced

orderInvariance :: Property
orderInvariance = property do
  numNames <- forAll $ Gen.int (Range.linear 2 4)
  let names = map (\n -> "x" <> Text.pack (show n)) [1..numNames]
  Just (SomePrimeW w@(PrimeW @p)) <- pure $ isPrime 5

  give w $ withVariables names \(vs :: [Polynomial (GF p) v Lex]) -> do
    polys <- forAll $ filter (/=0) <$> Gen.list (Range.exponential 2 4) (genPoly genGF vs)

    let calcGrevlex :: (PolynomialConstraint (Polynomial f v o))
                => [Polynomial f v o] -> [Polynomial f v DegRevLex]
        calcGrevlex  = autoReduce . groebnerBasis . map (withOrder DegRevLex)
        calcLex :: (PolynomialConstraint (Polynomial f v o))
                => [Polynomial f v o] -> [Polynomial f v Lex]
        calcLex      = autoReduce . groebnerBasis . map (withOrder Lex)
        basisGrevlex = calcGrevlex polys
        basisLex     = calcLex polys

    annotateShow basisGrevlex
    annotateShow basisLex
    annotateShow (calcLex basisGrevlex)

    assert $ equiv basisLex (calcLex basisGrevlex)

gcdRefl :: Property
gcdRefl = property do
  numNames <- forAll $ Gen.int (Range.linear 1 4)
  let names = map (\n -> "x" <> Text.pack (show n)) [1..numNames]

  withVariables names \(vs :: [Polynomial Q v Lex]) -> do
    poly <- normalize <$> forAll (genPoly genQ vs)
    let gcdRes = Algo.gcd poly poly

    annotateShow poly
    annotateShow gcdRes

    assert $ gcdRes == poly

gcdisgcd :: Property
gcdisgcd = withTests 1000 $ property do
  numNames <- forAll $ Gen.int (Range.linear 1 5)
  let names = map (\n -> "x" <> Text.pack (show n)) [1..numNames]

  withVariables names \(vs :: [Polynomial Q v Lex]) -> do
    commonDiv <- forAll $ Gen.filter (/=0) (genPoly genQ vs)
    annotateShow commonDiv
    lhs <- fmap (commonDiv*) $ forAll $ Gen.filter (/=0) (genPoly genQ vs)
    rhs <- fmap (commonDiv*) $ forAll $ Gen.filter (/=0) (genPoly genQ vs)

    annotateShow lhs
    annotateShow rhs

    let gcdRes = Algo.gcd lhs rhs

    annotateShow gcdRes

    assert $ leadReduceBySet [commonDiv] gcdRes == 0
    assert $ leadReduceBySet [gcdRes] lhs == 0
    assert $ leadReduceBySet [gcdRes] rhs == 0

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

