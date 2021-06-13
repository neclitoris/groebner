{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Units
  ( units
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Poly.Algorithms
import Poly.Fields
import Poly.Ideals
import Poly.Monomial
import Poly.Monomial.Order
import Poly.Polynomial

import Util


units :: TestTree
units = testGroup "units"
  [ testGroup "root4"
    [ root4
    , root4grevlex
    ]
  , testGroup "cyclic4"
    [ cyclic4
    , cyclic4grevlex
    ]
  , testGroup "katsura4"
    [ katsura4
    , katsura4grevlex
    ]
  ]

root4 :: TestTree
root4 = testCase "lex" $ withVariables ["x1", "x2", "x3", "x4"] f
  where
    f (vs@[x1, x2, x3, x4] :: [Polynomial Q v Lex])
      = basis @?= expected
      where
        basis = Equiv $ autoReduce $ groebnerBasis $ root vs
        expected = Equiv [ x1 + x2 + x3 + x4
                   , x2^2 + x2*x3 + x2*x4 + x3^2 + x3*x4 + x4^2
                   , x3^3 + x3^2*x4 + x3*x4^2 + x4^3
                   , x4^4 - 1
                   ]

root4grevlex :: TestTree
root4grevlex = testCase "grevlex" $ withVariables ["x1", "x2", "x3", "x4"] f
  where
    f (vs@[x1, x2, x3, x4] :: [Polynomial Q v Lex])
      = basis @?= expected
      where
        basis = Equiv $ map (withOrder Lex) $ autoReduce $ groebnerBasis
                  $ map (withOrder DegRevLex) $ root vs
        expected = Equiv [ x1 + x2 + x3 + x4
                   , x2^2 + x2*x3 + x3^2 + x2*x4 + x3*x4 + x4^2
                   , x3^3 + x3^2*x4 + x3*x4^2 + x4^3
                   , x4^4 - 1
                   ]

cyclic4 :: TestTree
cyclic4 = testCase "lex" $ withVariables ["x1", "x2", "x3", "x4"] f
  where
    f (vs@[x1, x2, x3, x4] :: [Polynomial Q v Lex])
      = basis @?= expected
      where
        basis = Equiv $ autoReduce $ groebnerBasis $ cyclic vs
        expected = Equiv [ x1 + x2 + x3 + x4
                   , x2^2 + 2*x2*x4 + x4^2
                   , x2*x4^4 - x2 + x4^5 - x4
                   , x3^3*x4^2 + x3^2*x4^3 - x3 - x4
                   , x2*x3 - x2*x4 + x3^2*x4^4 + x3*x4 - 2*x4^2
                   , x3^2*x4^6 - x3^2*x4^2 - x4^4 + 1
                   ]

cyclic4grevlex :: TestTree
cyclic4grevlex = testCase "grevlex" $ withVariables ["x1", "x2", "x3", "x4"] f
  where
    f (vs@[x1, x2, x3, x4] :: [Polynomial Q v Lex])
      = basis @?= expected
      where
        basis = Equiv $ map (withOrder Lex) $ autoReduce $ groebnerBasis
                  $ map (withOrder DegRevLex) $ cyclic vs
        expected = Equiv [ x1 + x2 + x3 + x4
                   , x2^2 + 2*x2*x4 + x4^2
                   , x2*x3^2 + x3^2*x4 - x2*x4^2 - x4^3
                   , x2*x3*x4^2 + x3^2*x4^2 - x2*x4^3 + x3*x4^3 - x4^4 - 1
                   , x2*x4^4 + x4^5 - x2 - x4
                   , x3^3*x4^2 + x3^2*x4^3 - x3 - x4
                   , x3^2*x4^4 + x2*x3 - x2*x4 + x3*x4 - 2*x4^2
                   ]

katsura4 :: TestTree
katsura4 = testCase "lex" $ withVariables ["x1", "x2", "x3", "x4"] f
  where
    f (vs@[x1, x2, x3, x4] :: [Polynomial Q v Lex])
      = basis @?= expected
      where
        basis = Equiv $ autoReduce $ groebnerBasis $ katsura vs
        expected = Equiv [ x1 - 53230079232 %% 1971025*x4^7 + 10415423232 %% 1971025*x4^6
                     + 9146536848 %% 1971025*x4^5 - 2158574456 %% 1971025*x4^4
                     - 838935856 %% 5913075*x4^3 + 275119624 %% 5913075*x4^2 + 4884038 %% 5913075*x4
                     - 1 %% 1
                   , x2 - 97197721632 %% 1971025*x4^7 + 73975630752 %% 1971025*x4^6
                     - 12121915032 %% 1971025*x4^5 - 2760941496 %% 1971025*x4^4
                     + 814792828 %% 1971025*x4^3 - 1678512 %% 1971025*x4^2 - 9158924 %% 1971025*x4
                   , x3 + 123812761248 %% 1971025*x4^7 - 79183342368 %% 1971025*x4^6
                     + 7548646608 %% 1971025*x4^5 + 3840228724 %% 1971025*x4^4
                     - 2024910556 %% 5913075*x4^3 - 132524276 %% 5913075*x4^2 + 30947828 %% 5913075*x4
                   , x4^8 - 8 %% 11*x4^7 + 4 %% 33*x4^6 + 131 %% 5346*x4^5 - 70 %% 8019*x4^4
                     + 1 %% 3564*x4^3 + 5 %% 42768*x4^2 - 1 %% 128304*x4
                   ]

katsura4grevlex :: TestTree
katsura4grevlex = testCase "grevlex" $ withVariables ["x1", "x2", "x3", "x4"] f
  where
    f (vs@[x1, x2, x3, x4] :: [Polynomial Q v Lex])
      = basis @?= expected
      where
        basis = Equiv $ map (withOrder Lex) $ autoReduce $ groebnerBasis
                  $ map (withOrder DegRevLex) $ katsura vs
        expected = Equiv [ x1 + 2 %% 1*x2 + 2 %% 1*x3 + 2 %% 1*x4 - 1 %% 1
                   , x2^2 + 2 %% 1*x2*x4 + 8 %% 7*x3*x4 + 12 %% 7*x4^2 - 2 %% 7*x2 - 1 %% 7*x3 - 4 %% 7*x4
                   , x2*x3 - 2 %% 1*x2*x4 - 23 %% 7*x3*x4 - 24 %% 7*x4^2 + 1 %% 14*x2 + 2 %% 7*x3
                     + 8 %% 7*x4
                   , x3^2 + 2 %% 1*x2*x4 + 32 %% 7*x3*x4 + 27 %% 7*x4^2 - 1 %% 7*x2 - 4 %% 7*x3 - 9 %% 7*x4
                   , x2*x4^2 - 1 %% 3*x4^3 - 1 %% 9*x2*x4 + 1 %% 54*x3*x4 + 1 %% 9*x4^2 - 1 %% 36*x2
                     - 1 %% 27*x3
                   , x3*x4^2 + 10 %% 9*x4^3 - 1 %% 18*x2*x4 - 17 %% 81*x3*x4 - 13 %% 27*x4^2 + 1 %% 54*x2
                     + 5 %% 162*x3 + 1 %% 27*x4
                   , x4^4 - 362 %% 891*x4^3 + 37 %% 891*x2*x4 + 1841 %% 16038*x3*x4 + 206 %% 2673*x4^2
                     - 13 %% 10692*x2 - 389 %% 32076*x3 - 47 %% 2673*x4
                   ]
