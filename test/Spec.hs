import Test.HUnit qualified as HU

import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

import Poly.Algorithms
import Poly.Fields
import Poly.Ideals
import Poly.Monomial
import Poly.Monomial.Order
import Poly.Monomial.Variables
import Poly.Polynomial

import Properties
import Units


tests :: Tasty.TestTree
tests = Tasty.testGroup "tests" [ properties, units ]

main :: IO ()
main = Tasty.defaultMain tests
