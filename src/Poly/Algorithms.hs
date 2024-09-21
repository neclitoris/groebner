{-# LANGUAGE ScopedTypeVariables #-}
module Poly.Algorithms
  (
  -- * Reductions
    leadReduceBy
  , maybeLeadReduceBy
  , fullyReduceBy
  , maybeFullyReduceBy
  , reduceBySet
  , leadReduceBySet
  , fullyReduceBySet

  -- * Algorithms
  , sPolynomial
  , groebnerBasis
  , autoReduce
  , gcd
  ) where

import Control.Monad
import Control.Monad.Trans.Cont (evalCont, reset, shift)

import Data.Maybe
import Data.Function
import Data.List
import Data.List.Singletons
import Data.Singletons
import GHC.TypeLits

import Debug.Trace

import Poly.Monomial hiding (variables)
import Poly.Polynomial


import Prelude hiding (div, gcd, lcm)


-- | Lead-reduction. That is, if \(\operatorname{LM}(g) \mid \operatorname{LM}(f)\)
-- , compute \(f - \frac{\operatorname{LM}(f)}{\operatorname{LM}(g)} \cdot g\).
maybeLeadReduceBy
  :: PolynomialConstraint (Polynomial f v o)
  => Polynomial f v o -- ^ Divisor
  -> Polynomial f v o -- ^ Dividend
  -> Maybe (Polynomial f v o)
maybeLeadReduceBy divisor dividend = do
  m <- leading dividend
  n <- leading divisor
  r <- divide m n
  pure $ dividend - toPolynomial r * divisor

-- | Lead reduction. Fails on zero divisor. If impossible to reduce, returns dividend.
leadReduceBy :: PolynomialConstraint (Polynomial f v o)
             => Polynomial f v o -- ^ Divisor
             -> Polynomial f v o -- ^ Dividend
             -> Polynomial f v o
leadReduceBy divisor dividend = evalCont $ reset do
  m <- maybe (shift \_ -> return 0) return $ leading dividend
  n <- maybe (shift \_ -> error "Division by zero") return $ leading divisor
  r <- maybe (shift \_ -> return dividend) return $ divide m n
  pure $ dividend - toPolynomial r * divisor

-- | Reduction. That is, if \(\operatorname{LM}(g) \mid m\) and \(m \in f\)
-- , compute \(f - \frac{m}{\operatorname{LM}(g)} \cdot g\).
maybeFullyReduceBy
  :: PolynomialConstraint (Polynomial f v o)
  => Polynomial f v o -- ^ Divisor
  -> Polynomial f v o -- ^ Dividend
  -> Maybe (Polynomial f v o)
maybeFullyReduceBy divisor dividend = do
  lead <- leading divisor
  r    <- listToMaybe $ mapMaybe (`divide` lead) (monomials dividend)
  pure $ dividend - toPolynomial r * divisor

-- | Reduction. Fails on zero divisor. If impossible to reduce, returns dividend.
fullyReduceBy
  :: PolynomialConstraint (Polynomial f v o)
  => Polynomial f v o -- ^ Divisor
  -> Polynomial f v o -- ^ Dividend
  -> Polynomial f v o
fullyReduceBy divisor dividend = dividend - r * divisor
  where
    lead = fromMaybe (error "Division by zero") $ leading divisor
    r    = maybe 0 toPolynomial $ listToMaybe $ mapMaybe (`divide` lead)
             (monomials dividend)

-- | Reduce polynomial to its normal form w.r.t. a set, that is, reduce until
-- no reduction can be performed. Uses strategy passed as the first parameter.
reduceBySet
  :: PolynomialConstraint (Polynomial f v o)
  => (Polynomial f v o
      -> Polynomial f v o
      -> Maybe (Polynomial f v o)) -- ^ Strategy (lead/full reduction)
  -> [Polynomial f v o]            -- ^ Set of divisors
  -> Polynomial f v o              -- ^ Dividend
  -> Polynomial f v o
reduceBySet strategy divisors dividend = r
  where
    r     = last $ map fromJust (takeWhile isJust forms)
    forms = iterate (>>= go) (Just dividend)
    go p  = listToMaybe $ mapMaybe (`strategy` p) divisors

-- | Reduce polynomial to lead-normal form w.r.t. a set.
leadReduceBySet :: PolynomialConstraint (Polynomial f v o)
                => [Polynomial f v o] -- ^ Set of divisors
                -> Polynomial f v o   -- ^ Dividend
                -> Polynomial f v o
leadReduceBySet = reduceBySet maybeLeadReduceBy

-- | Reduce polynomial to normal form w.r.t. a set.
fullyReduceBySet :: PolynomialConstraint (Polynomial f v o)
                 => [Polynomial f v o] -- ^ Set of divisors
                 -> Polynomial f v o   -- ^ Dividend
                 -> Polynomial f v o
fullyReduceBySet = reduceBySet maybeFullyReduceBy

div :: PolynomialConstraint (Polynomial f v o)
    => Polynomial f v o
    -> Polynomial f v o
    -> Polynomial f v o
div dividend divisor
  | r == 0 = 0
  | otherwise = r + div (dividend - r * divisor) divisor
  where
    lead = fromMaybe (error "Division by zero") $ leading divisor
    r    = maybe 0 toPolynomial $ listToMaybe $ mapMaybe (`divide` lead)
             (monomials dividend)

-- | Compute s-polynomial. That is a polynomial
-- \[
--  \operatorname{lc}(g)\frac{\operatorname{lcm}(\operatorname{lm}(f),
--  \operatorname{lm}(g))}{\operatorname{lm}(f)} \cdot f
--  - \operatorname{lc}(f)\frac{\operatorname{lcm}(\operatorname{lm}(f),
--  \operatorname{lm}(g))}{\operatorname{lm}(g)} \cdot g.
-- \]
-- Fails on zero inputs.
sPolynomial :: PolynomialConstraint (Polynomial f v o)
            => Polynomial f v o -> Polynomial f v o -> Polynomial f v o
sPolynomial g f = fromJust $ do
  m <- leading g
  n <- leading f
  let r = lcm m n
  pure $ toPolynomial (lcmLhsMult r) * g
       - toPolynomial (lcmRhsMult r) * f

-- | Compute Gröbner basis of the ideal generated by a set of polynomials.
groebnerBasis :: PolynomialConstraint (Polynomial f v o)
              => [Polynomial f v o] -> [Polynomial f v o]
groebnerBasis = fst . head . dropWhile (not . null . snd) . iters
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
        0 -> (have, new)
        s -> (have ++ [fullyReduceBySet have s], new ++ sPolys have s)
    go have []      = (have, [])

    iters gens = iterate (uncurry go)
                         (gens
                         , [ s | (f:gs) <- tails gens, g <- gs
                           , s <- maybeToList $ maybeSPoly f g ])

-- | Reduce each polynomial to a normal form w.r.t. all others.
-- Exclude zeroes.
autoReduce :: PolynomialConstraint (Polynomial f v o)
           => [Polynomial f v o] -> [Polynomial f v o]
autoReduce = reduceP []
  where
    reduceP before (f:after) =
      case fullyReduceBySet (before ++ after) f of
        0  -> reduceP before after
        f' -> reduceP (before ++ [normalize f']) after
    reduceP before []        = before

gcdUni :: (PolynomialConstraint (Polynomial f v o), v ~ '[x])
       => [Polynomial f v o] -> Polynomial f v o
gcdUni = foldl1' gcd2
  where
    gcd2 l r
      | r == 0 = l
      | otherwise = gcd2 r (leadReduceBy r l)

gcd :: forall f v o . PolynomialConstraint (Polynomial f v o)
    => Polynomial f v o -> Polynomial f v o -> Polynomial f v o
gcd lhs rhs
  | lhs == 0 || rhs == 0 = 0
  | otherwise =
  let lcm = withSingI (sing @"t" `SCons` sing @v)
            withOrder order . uwkn . head $
            filter (maybe True ((== 0) . head . powers) . leading) $
            autoReduce $
            groebnerBasis $ map chorder [t * wkn lhs, (1 - t) * wkn rhs]
              where
                chorder = withOrder (order :: Elim 1 DegRevLex)
                (t:_) = variables @("t":v)
                wkn :: forall o' . PolynomialConstraint (Polynomial f v o')
                    => Polynomial f v o' -> Polynomial f ("t":v) o'
                wkn   = sum . map (toPolynomial . \(Monomial c p) -> Monomial @f @("t":v) @o' c (0:p)) . monomials
                uwkn :: forall o'. PolynomialConstraint (Polynomial f v o')
                     => Polynomial f ("t":v) o' -> Polynomial f v o'
                uwkn  = sum . map (toPolynomial . \(Monomial c p) -> Monomial @f @v @o' c (drop 1 p)) . monomials
   in (lhs * rhs) `div` lcm


