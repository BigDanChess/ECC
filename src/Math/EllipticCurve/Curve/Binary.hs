{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Binary
-- Description : Curvas sobre campos binarios
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definición de curvas elípticas sobre campos binarios.
module Math.EllipticCurve.Curve.Binary
  ( module Math.EllipticCurve.Curve,
    Point (..),

    -- * Binary curves
    BCurve (..),
    BPoint,

    -- ** Curvas afines binarias
    BACurve (..),
    BAPoint,
  )
where

import Control.Monad.Random (Random)
import Data.Group (Group)
import GHC.Natural (Natural)
import Math.Algebra.Galois as F (GaloisField, PrimeField, frob)
import Math.EllipticCurve.Curve
import Protolude
import Test.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty (..))

-- * Forma binaria

-- | Puntos binarios.
type BPoint = Point 'Binary

-- | Curvas binarias.
class
  (GaloisField q, PrimeField r, Curve 'Binary c e q r) =>
  BCurve c e q r
  where
  {-# MINIMAL a_, b_, h_, p_, r_ #-}
  a_ ::
    BPoint c e q r ->
    -- | Coeficiente @A@.
    q
  b_ ::
    BPoint c e q r ->
    -- | Coeficiente @B@.
    q
  h_ ::
    BPoint c e q r ->
    -- | Cofactor de Curva.
    Natural
  p_ ::
    BPoint c e q r ->
    -- | Polinomio de curva.
    Natural
  r_ ::
    BPoint c e q r ->
    -- | Orden de curva.
    Natural

-- *  Coordenadas afines

-- | Puntos afines binarios.
type BAPoint = BPoint 'Affine

-- | Curvas afines binarias \(y^2 + xy = x^3 + Ax^2 + B\).
class (BCurve 'Affine e q r) => BACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ ::
    -- | Generador de curva.
    BAPoint e q r

-- | Las curvas afines binarias son curvas elípticas.
instance (BACurve e q r) => Curve 'Binary 'Affine e q r where
  data Point 'Binary 'Affine e q r
    = A q q
    | -- \^ Punto afín.
      O
    -- \^ Punto en el infinito.
    deriving (Eq, Generic, NFData, Read, Show)

  add ::
    Point 'Binary 'Affine e q r ->
    Point 'Binary 'Affine e q r ->
    Point 'Binary 'Affine e q r
  add p O = p
  add O q = q
  add p@(A x1 y1) q@(A x2 y2)
    | xx == 0 && yy + x2 == 0 = O -- Si Q es la inversa de P
    | x1 == x2 = dbl p
    | otherwise = A x3 y3
    where
      a = a_ (witness :: BAPoint e q r)
      xx = x1 + x2
      yy = y1 + y2
      l = yy / xx
      x3 = l * (l + 1) + xx + a
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINEABLE add #-}

  char :: Point Binary Affine e q r -> Natural
  char = const 2
  {-# INLINEABLE char #-}

  cof :: Point Binary Affine e q r -> Natural
  cof = h_
  {-# INLINEABLE cof #-}

  dbl :: Point Binary Affine e q r -> Point Binary Affine e q r
  dbl O = O
  dbl (A x y) = A x' y'
    where
      a = a_ (witness :: BAPoint e q r)
      l = x + y / x
      l' = l + 1
      x' = l * l' + a
      y' = x * x + l' * x'
  {-# INLINEABLE dbl #-}

  def :: Point Binary Affine e q r -> Bool
  def O = True
  def (A x y) = ((x + a) * x + y) * x + b + y * y == 0
    where
      a = a_ (witness :: BAPoint e q r)
      b = b_ (witness :: BAPoint e q r)
  {-# INLINEABLE def #-}

  disc :: Point Binary Affine e q r -> q
  disc _ = b_ (witness :: BAPoint e q r)
  {-# INLINEABLE disc #-}

  frob :: Point Binary Affine e q r -> Point Binary Affine e q r
  frob O = O
  frob (A x y) = A (F.frob x) (F.frob y)
  {-# INLINEABLE frob #-}

  gen :: Point Binary Affine e q r
  gen = gA_
  {-# INLINEABLE gen #-}

  id :: Point Binary Affine e q r
  id = O
  {-# INLINEABLE id #-}

  inv :: Point Binary Affine e q r -> Point Binary Affine e q r
  inv O = O
  inv (A x y) = A x (x + y)
  {-# INLINEABLE inv #-}

  order :: Point Binary Affine e q r -> Natural
  order = r_
  {-# INLINEABLE order #-}
