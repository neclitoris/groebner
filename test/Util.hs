{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util
  ( equiv
  , EquivList(..)
  ) where

import Data.Function


equiv :: Eq a => [a] -> [a] -> Bool
equiv xs ys  = and [ x `elem` ys && x `elem` xs | x <- xs ++ ys ]

newtype EquivList a = Equiv { getEquiv :: [a] }
  deriving newtype (Show)

instance Eq a => Eq (EquivList a) where
  (==) = equiv `on` getEquiv
