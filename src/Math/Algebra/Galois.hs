-- |
-- Module      : Galois
-- Description : Importaciones para las clases de Galois
-- Copyright   : (c) Adjoint Inc.
--               (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Instancia de curva elíptica SECT11ER1.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Implementación de clases para campos de Galois
module Math.Algebra.Galois
  ( module Math.Algebra.GaloisFields.Base,

    -- ** Campos Primos
    module Math.Algebra.GaloisFields.Prime,

    -- ** Campos binarios
    module Math.Algebra.GaloisFields.Binary,

    rnd 
  )
where

import Data.Group (Group (invert))
import Data.Poly (Poly (unPoly), VPoly)
import Data.Word (Word64)
import GHC.IsList (IsList (toList))
import GHC.Natural (naturalToInteger)
import Math.Algebra.GaloisFields.Base
import Math.Algebra.GaloisFields.Binary
import Math.Algebra.GaloisFields.Prime
import System.Random
import Text.PrettyPrint.Leijen.Text (Pretty (pretty))
import Control.Monad.Random (MonadRandom, StdGen, getRandom, getRandomR, mkStdGen, runRand)

-- | Elemento aleatorio de un campo.
rnd :: (GaloisField k, MonadRandom m) => m k
rnd = getRandom
{-# INLINEABLE rnd #-}