module Poly.Algorithms
  ( reduceBy
  , maybeReduceBy
  , reduceBySet
  , sPoly
  , groebnerBasis
  ) where

import Control.Applicative ((<|>))
import Control.Monad (join, (<=<))
import Control.Monad.Trans.Cont (evalCont, reset, shift)

import Data.Maybe
import Data.List

import Poly.Monomial
import Poly.Polynomial

import Prelude hiding (lcm)


maybeReduceBy
  :: PolynomialConstraint (Polynomial f v o)
  => Polynomial f v o -> Polynomial f v o -> Maybe (Polynomial f v o)
maybeReduceBy divisor dividend = do
  m <- leading dividend
  n <- leading divisor
  r <- divide m n
  pure $ dividend - toPolynomial r * divisor

reduceBy :: PolynomialConstraint (Polynomial f v o)
         => Polynomial f v o -> Polynomial f v o -> Polynomial f v o
reduceBy divisor dividend = evalCont $ reset do
  m <- maybe (shift \_ -> return 0) return $ leading dividend
  n <- maybe (shift \_ -> error "Division by zero") return $ leading divisor
  r <- maybe (shift \_ -> return dividend) return $ divide m n
  pure $ dividend - toPolynomial r * divisor

reduceBySet :: PolynomialConstraint (Polynomial f v o)
            => [Polynomial f v o] -> Polynomial f v o -> Polynomial f v o
reduceBySet divisors dividend = r
  where
    r     = last $ map fromJust (takeWhile isJust forms)
    forms = iterate (>>= go) (Just dividend)
    go p  = listToMaybe $ catMaybes [ maybeReduceBy d p | d <- divisors ]

sPoly :: PolynomialConstraint (Polynomial f v o)
      => Polynomial f v o -> Polynomial f v o -> Polynomial f v o
sPoly g f = fromJust $ do
  m <- leading g
  n <- leading f
  let r = lcm m n
  pure $ toPolynomial (lcmLhsMult r) * g
       - toPolynomial (lcmRhsMult r) * f

groebnerBasis :: PolynomialConstraint (Polynomial f v o)
              => [Polynomial f v o] -> [Polynomial f v o]
groebnerBasis gens = go gens [ s | (f:gs) <- tails gens, g <- gs
                                   , let s = sPoly f g, s /= 0 ]
  where
    sPolys set new = [ s | f <- set, let s = sPoly f new, s /= 0 ]

    go have (n:new) =
      case reduceBySet have n of
        0 -> go have new
        s -> go (s : have) (sPolys have s ++ new)
    go have []      = have
