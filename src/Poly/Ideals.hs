module Poly.Ideals
  ( cyclic
  , root
  , katsura
  ) where

import Data.List

import Poly.Polynomial

cyclic :: PolynomialConstraint (Polynomial f v o)
       => [Polynomial f v o] -> [Polynomial f v o]
cyclic p = [ s | n <- [1..length p - 1]
               , let s = sum
                           $ take (length p)
                           $ map (product . take n)
                           $ tails
                           $ cycle p
           ] ++ [product p - 1]

root :: PolynomialConstraint (Polynomial f v o)
     => [Polynomial f v o] -> [Polynomial f v o]
root p = map sigma [1..n - 1] ++ [sigma n - (-1)^(n - 1)]
  where
    n = length p
    sigma m = sum $ map product
      $ filter ((==m) . length)
      $ subsequences p

katsura :: PolynomialConstraint (Polynomial f v o)
        => [Polynomial f v o] -> [Polynomial f v o]
katsura p = sum [ f l | l <- [-n..n] ] - 1 :
    [ sum [ f l * f (m - l) | l <- [-n..n] ] - f m | m <- [0..n - 1] ]
  where
    f x
      | x < 0     = f (-x)
      | x <= n    = p !! x
      | otherwise = 0
    n = length p - 1
