{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Poly.Primes where

import Control.Monad
import Language.Haskell.TH

import Poly.Fields

$(join <$> mapM (recover (pure []) . assertPrimality) [2..127])
