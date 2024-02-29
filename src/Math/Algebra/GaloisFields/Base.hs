{-|
Module      : Base
Description : Clase de Campo de Galois
Copyright   : (c) Adjoint Inc. 
              (c) Daniel Alejandro Reinoso, 2024
License     : GPL-3
Maintainer  : danixreinoso@gmail.com
Stability   : experimental
Portability : POSIX

Definición de campo de Galois.
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Math.Algebra.GaloisFields.Base where

import Control.Monad.Random (Random)
import Data.Field (Field)
import Data.Group qualified as G (Group (..))
import GHC.TypeLits (Natural)
import Protolude hiding (one, quot, (-))
import Test.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Clases
-------------------------------------------------------------------------------

-- | Campo de Galois \(\mathbb{F}_{q}\) con \(q\) de la forma \(q=p^n\) con \(p\) primo y \(n\) entero positivo.
class
  ( Field k,
    G.Group k,
    Fractional k,
    Generic k,
    NFData k,
    Ord k,
    Random k,
    Show k
  ) =>
  GaloisField k   
  where
  {-# MINIMAL char, deg, frob #-}

  -- | Característica \(p\) del campo y orden del subcampo primo.
  char :: k -> Natural

  -- | Grado \(n\) del campo como un campo de extensión sobre su subcampo primo.
  deg :: k -> Word

  -- | Automorfismo de Frobenius \(x \to x^p\) del subcampo primo.
  frob :: k -> k

  -- | Orden \(q=p^n\) del campo.
  order :: k -> Natural
  order = (^) <$> char <*> deg
  {-# INLINEABLE order #-}


-- | Exponenciación entera de un elemento del campo.
pow :: (GaloisField k, Integral n) => k -> n -> k
pow = G.pow
{-# INLINEABLE pow #-}
{-# SPECIALIZE pow ::
  (GaloisField k) => k -> Int -> k,
  (GaloisField k) => k -> Integer -> k,
  (GaloisField k) => k -> Natural -> k,
  (GaloisField k) => k -> Word -> k
  #-}