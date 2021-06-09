import Test.Tasty qualified as Tasty

import Properties
import Units


tests :: Tasty.TestTree
tests = Tasty.testGroup "tests" [ properties, units ]

main :: IO ()
main = Tasty.defaultMain tests
