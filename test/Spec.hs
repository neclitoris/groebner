{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty

import Poly.Algorithms
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
buchbergerCriterion = withDiscards 10 $ withTests 100 $ property do
  names <- forAll $ nub <$> Gen.list (Range.linear 2 10) genName

  withVariables names \(vs :: [Polynomial (Ratio Integer) v Lex]) -> do
    polys <- forAll $ filter (/=0) <$> Gen.list (Range.exponential 2 100) (genPoly genQ vs)

    let basis   = groebnerBasis polys
        sPolys  = [ s | (f:gs) <- tails basis, g <- gs
                      , let s = sPoly f g, s /= 0]
        reduced = map (reduceBySet basis) sPolys
    -- liftIO $ print [polys, basis]
    assert $ all (==0) reduced


genName :: MonadGen m => m Text
genName = Gen.text (Range.constant 1 1) Gen.lower

genQ :: MonadGen m => m (Ratio Integer)
genQ = (%)
  <$> Gen.integral (Range.exponential (-10) 10)
  <*> Gen.integral (Range.exponential 1 10)

genPoly :: forall v f m . (PolynomialConstraint (Polynomial f v Lex), MonadGen m)
        => m f
        -> [Polynomial f v Lex]
        -> m (Polynomial f v Lex)
genPoly mCoef vars = do
  monos <- Gen.list (Range.exponential 1 100) (genMono mCoef vars)
  pure $ sum monos

genMono :: forall v f m . (PolynomialConstraint (Polynomial f v Lex), MonadGen m)
        => m f
        -> [Polynomial f v Lex]
        -> m (Polynomial f v Lex)
genMono mCoef vars = do
  coef   <- mCoef
  powers <- replicateM (numVars @v) $ Gen.int $ Range.linear 0 20
  pure $ toPolynomial coef * product (zipWith (^) vars powers)

numVars :: forall (v :: Vars). SingI v => Int
numVars = length $ fromSing (sing :: Sing v)


main :: IO ()
main = void $ checkSequential $ Group "properties" [ ("buchberger criterion", buchbergerCriterion) ]
