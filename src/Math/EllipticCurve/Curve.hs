{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Curve
-- Description : Curva elíptica
-- Copyright   : (c) Adjoint Inc.
--               (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definición de curvas elípticas.

module Math.EllipticCurve.Curve (module Math.EllipticCurve.Curve) where

import Control.Monad.Random (MonadRandom, Random (..), RandomGen, getRandom)
import Data.Group (Group (..))
import GHC.Natural (Natural)
import Math.Algebra.Galois (GaloisField, PrimeField, fromP)
import Protolude
import Test.QuickCheck (Arbitrary (..), Gen)
import Text.PrettyPrint.Leijen.Text (Pretty)

-- * Tipos

-- | Curvas elípticas
class
  ( GaloisField q,
    PrimeField r,
    Eq (Point f c e q r),
    Generic (Point f c e q r),
    Group (Point f c e q r),
    NFData (Point f c e q r),
    Random (Point f c e q r),
    Show (Point f c e q r)
  ) =>
  Curve (f :: Form) (c :: Coordinates) e q r
  where
  {-# MINIMAL
    add,
    char,
    cof,
    dbl,
    def,
    disc,
    frob,
    gen,
    id,
    inv,
    order
    #-}

  -- | Punto de curva.
  data Point f c e q r :: Type

  -- * Parámetros

  -- | Característica de la curva.
  char :: Point f c e q r -> Natural

  -- | Cofactor de la curva.
  cof :: Point f c e q r -> Natural

  -- | Determina si el punto dado pertenece a la curva elíptica.
  def :: Point f c e q r -> Bool

  -- | Discriminante de la curva.
  disc :: Point f c e q r -> q

  -- | Orden de la curva.
  order :: Point f c e q r -> Natural

  -- * Operaciones

  -- | Suma de puntos.
  add :: Point f c e q r -> Point f c e q r -> Point f c e q r

  -- | Doblaje de puntos.
  dbl :: Point f c e q r -> Point f c e q r

  -- | Punto identidad.
  id :: Point f c e q r

  -- | Inversión de puntos.
  inv :: Point f c e q r -> Point f c e q r

  -- * Funciones

  -- | Endomorfismo de Frobenius
  frob :: Point f c e q r -> Point f c e q r


  -- | Generador de curva.
  gen :: Point f c e q r

  -- | Punto aleatorio.
  rnd :: (MonadRandom m) => m (Point f c e q r)
  rnd = getRandom


-- | Multiplicación de un punto por un elemento del campo
mul :: (Curve f c e q r) => Point f c e q r -> r -> Point f c e q r
mul = (. fromP) . mul'
{-# INLINEABLE mul #-}

-- | Multiplicación de un punto por un elemento integral.
mul' ::
  (Curve f c e q r, Integral n) =>
  Point f c e q r ->
  n ->
  Point f c e q r
mul' p n
  | n < 0 = inv $ mul' p (-n)
  | n == 0 = id
  | n == 1 = p
  | even n = p'
  | otherwise = add p p'
  where
    p' = mul' (dbl p) (div n 2)
{-# INLINEABLE mul' #-}
{-# SPECIALIZE mul' ::
  (Curve f c e q r) => Point f c e q r -> Int -> Point f c e q r,
  (Curve f c e q r) => Point f c e q r -> Integer -> Point f c e q r,
  (Curve f c e q r) => Point f c e q r -> Natural -> Point f c e q r,
  (Curve f c e q r) => Point f c e q r -> Word -> Point f c e q r
  #-}

-- | Forma de la curva.
data Form
  = Binary
  | Binary2
  | Weierstrass

-- | Coordenadas de la curva.
data Coordinates
  = Affine


-- * Instancias


-- | Los puntos de curva elíptica forma un semigrupo.
instance (Curve f c e q r) => Semigroup (Point f c e q r) where
  (<>) :: Point f c e q r -> Point f c e q r -> Point f c e q r
  p <> q = if p == q then dbl p else add p q
  {-# INLINEABLE (<>) #-}

-- | Los puntos de curva elíptica formaun un monoide.
instance (Curve f c e q r) => Monoid (Point f c e q r) where
  mempty :: Point f c e q r
  mempty = id
  {-# INLINEABLE mempty #-}

-- | Los puntos de curva elíptica son grupos
instance (Curve f c e q r) => Group (Point f c e q r) where
  invert :: Point f c e q r -> Point f c e q r
  invert = inv
  {-# INLINEABLE invert #-}

  pow :: (Integral x) => Point f c e q r -> x -> Point f c e q r
  pow = mul'
  {-# INLINEABLE pow #-}


-- | Los puntos de curva elíptica son aleatorios.
instance (Curve f c e q r) => Random (Point f c e q r) where
  -- Elemento aleatorio del grupo.
  random :: (RandomGen g) => g -> (Point f c e q r, g)
  random = first (mul gen) . random

  {-# INLINEABLE random #-}

  randomR :: (Point f c e q r, Point f c e q r) -> g -> (Point f c e q r, g)
  randomR = panic "Curve.randomR: no implementado."
