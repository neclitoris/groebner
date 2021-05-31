module Poly.Algorithms
  ( leadReduceBy
  , maybeLeadReduceBy
  , fullyReduceBy
  , maybeFullyReduceBy
  , reduceBySet
  , leadReduceBySet
  , fullyReduceBySet
  , sPolynomial
  , groebnerBasis
  , autoReduce
  ) where

import Control.Monad
import Control.Monad.Trans.Cont (evalCont, reset, shift)

import Data.Maybe
import Data.List

import Poly.Monomial
import Poly.Polynomial

import Prelude hiding (lcm)


maybeLeadReduceBy
  :: PolynomialConstraint (Polynomial f v o)
  => Polynomial f v o -> Polynomial f v o -> Maybe (Polynomial f v o)
maybeLeadReduceBy divisor dividend = do
  m <- leading dividend
  n <- leading divisor
  r <- divide m n
  pure $ dividend - toPolynomial r * divisor

leadReduceBy :: PolynomialConstraint (Polynomial f v o)
         => Polynomial f v o -> Polynomial f v o -> Polynomial f v o
leadReduceBy divisor dividend = evalCont $ reset do
  m <- maybe (shift \_ -> return 0) return $ leading dividend
  n <- maybe (shift \_ -> error "Division by zero") return $ leading divisor
  r <- maybe (shift \_ -> return dividend) return $ divide m n
  pure $ dividend - toPolynomial r * divisor

maybeFullyReduceBy
  :: PolynomialConstraint (Polynomial f v o)
  => Polynomial f v o -> Polynomial f v o -> Maybe (Polynomial f v o)
maybeFullyReduceBy divisor dividend = do
  lead <- leading divisor
  r    <- listToMaybe $ catMaybes [ divide m lead | m <- monomials dividend ]
  pure $ dividend - toPolynomial r * divisor

fullyReduceBy
  :: PolynomialConstraint (Polynomial f v o)
  => Polynomial f v o -> Polynomial f v o -> Polynomial f v o
fullyReduceBy divisor dividend = dividend - r * divisor
  where
    lead = fromMaybe (error "Division by zero") $ leading divisor
    r    = maybe 1 toPolynomial $ listToMaybe $ catMaybes
            [ divide m lead | m <- monomials dividend ]

reduceBySet
  :: PolynomialConstraint (Polynomial f v o)
  => (Polynomial f v o -> Polynomial f v o -> Maybe (Polynomial f v o))
  -> [Polynomial f v o] -> Polynomial f v o -> Polynomial f v o
reduceBySet strategy divisors dividend = r
  where
    r     = last $ map fromJust (takeWhile isJust forms)
    forms = iterate (>>= go) (Just dividend)
    go p  = listToMaybe $ catMaybes [ strategy d p | d <- divisors ]

leadReduceBySet :: PolynomialConstraint (Polynomial f v o)
                => [Polynomial f v o] -> Polynomial f v o -> Polynomial f v o
leadReduceBySet = reduceBySet maybeLeadReduceBy

fullyReduceBySet :: PolynomialConstraint (Polynomial f v o)
                => [Polynomial f v o] -> Polynomial f v o -> Polynomial f v o
fullyReduceBySet = reduceBySet maybeFullyReduceBy

sPolynomial :: PolynomialConstraint (Polynomial f v o)
      => Polynomial f v o -> Polynomial f v o -> Polynomial f v o
sPolynomial g f = fromJust $ do
  m <- leading g
  n <- leading f
  let r = lcm m n
  pure $ toPolynomial (lcmLhsMult r) * g
       - toPolynomial (lcmRhsMult r) * f

groebnerBasis :: PolynomialConstraint (Polynomial f v o)
              => [Polynomial f v o] -> [Polynomial f v o]
groebnerBasis gens = go gens [ s | (f:gs) <- tails gens, g <- gs
                                 , s <- maybeToList $ maybeSPoly f g ]
  where
    sPolys set new = [ s | f <- set, s <- maybeToList $ maybeSPoly f new ]

    maybeSPoly f g = do
      m <- leading f
      n <- leading g
      let r = lcm m n
      guard (lcmLhsMult r /= n)
      pure $ toPolynomial (lcmLhsMult r) * f
           - toPolynomial (lcmRhsMult r) * g

    go have (n:new) =
      case leadReduceBySet have n of
        0 -> go have new
        s -> go (have ++ [fullyReduceBySet have s]) (new ++ sPolys have s)
    go have []      = have

autoReduce :: PolynomialConstraint (Polynomial f v o)
           => [Polynomial f v o] -> [Polynomial f v o]
autoReduce = reduceP []
  where
    reduceP before (f:after) =
      case fullyReduceBySet (before ++ after) f of
        0  -> reduceP before after
        f' -> reduceP (before ++ [abs f']) after
    reduceP before []        = before
