{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import Criterion
import Criterion.Main
import Data.Singletons
import Data.Text qualified as Text

import Poly.Algorithms
import Poly.Fields
import Poly.Ideals
import Poly.Monomial.Order
import Poly.Polynomial


$(assertPrimality 127)


mkVars i = map (\i -> "x" <> Text.pack (show i)) [1..i]


groebnerPipeline :: PolynomialConstraint (Polynomial (GF 127) v Lex)
                 => [Polynomial (GF 127) v Lex] -> [String]
groebnerPipeline =
  map (show . withOrder Lex)
  . autoReduce
  . groebnerBasis
  . map (withOrder (Graded RevLex))

groebnerPipelineToLex :: PolynomialConstraint (Polynomial (GF 127) v Lex)
                      => [Polynomial (GF 127) v Lex] -> [String]
groebnerPipelineToLex =
  map show
  . autoReduce
  . groebnerBasis
  . map (withOrder Lex)
  . autoReduce
  . groebnerBasis
  . map (withOrder (Graded RevLex))

run :: (forall v . SingI v => [Polynomial (GF 127) v Lex] -> [Polynomial (GF 127) v Lex])
    -> (forall v . SingI v => [Polynomial (GF 127) v Lex] -> r)
    -> Int
    -> r
run system pipeline n = withVariables (mkVars n) (pipeline . system)

main = defaultMain
  [ bgroup "find reduced Groebner basis, GF 127, grevlex"
    [ bgroup "cyclic" $
        map (\i -> bench ("cyclic" ++ show i)
                     $ nf (run cyclic groebnerPipeline) i)
            [3..5]
    , bgroup "root" $
        map (\i -> bench ("root" ++ show i)
                     $ nf (run root groebnerPipeline) i)
            [5..8]
    , bgroup "katsura" $
        map (\i -> bench ("katsura" ++ show i)
                     $ nf (run katsura groebnerPipeline) i)
            [4..6]
    ]
  ]
