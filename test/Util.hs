module Util
  ( equiv
  ) where


equiv :: Eq a => [a] -> [a] -> Bool
equiv xs ys  = and [ x `elem` ys && x `elem` xs | x <- xs ++ ys ]
