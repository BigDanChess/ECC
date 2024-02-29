{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Weierstrass
-- Description : Curvas sobre campos primos
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definición de curvas elípticas sobre cmpos primos con característica no igual a 2 o 3.
module Math.EllipticCurve.Curve.Weierstrass where

import Control.Monad.Random (Random)
import Data.Group (Group)
import GHC.Natural (Natural)
import Math.Algebra.Galois as F (GaloisField, PrimeField, frob)
import Math.EllipticCurve.Curve
import Protolude
import Test.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty (..))

-------------------------------------------------------------------------------
-- Forma de Weierstrass
-------------------------------------------------------------------------------

-- | Puntos de Weierstrass.
type WPoint = Point 'Weierstrass

-- | Curvas de Weierstrass.
class (GaloisField q, PrimeField r, Curve 'Weierstrass c e q r) => WCurve c e q r where
  {-# MINIMAL a_, b_, h_, q_, r_ #-}
  a_ ::
    WPoint c e q r ->
    -- | Coeficiente @A@.
    q
  b_ ::
    WPoint c e q r ->
    -- | Coeficiente @B@.
    q
  h_ ::
    WPoint c e q r ->
    -- | Cofactor de curva.
    Natural
  q_ ::
    WPoint c e q r ->
    -- | Característica de curva.
    Natural
  r_ ::
    WPoint c e q r ->
    -- | Orden de curva.
    Natural

-------------------------------------------------------------------------------
-- Coordenadas afines
-------------------------------------------------------------------------------

-- | Puntos afines de Weierstrass.
type WAPoint = WPoint 'Affine

-- | Curvas afines de Weierstrass @y^2 = x^3 + Ax + B@.
class (WCurve 'Affine e q r) => WACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ ::
    -- | Generador de curva
    WAPoint e q r

-- Las curvas afines de Weierstrass son curvas elípticas.
instance (WACurve e q r) => Curve 'Weierstrass 'Affine e q r where
  data Point 'Weierstrass 'Affine e q r
    = A q q
    | -- \^ Punto afín.
      O
    -- \^ Punto en el infinito.
    deriving (Eq, Generic, NFData, Read, Show)

  add ::
    Point Weierstrass Affine e q r ->
    Point Weierstrass Affine e q r ->
    Point Weierstrass Affine e q r
  add p O = p
  add O q = q
  add (A x1 y1) (A x2 y2)
    | x1==x2 && y1==y2 = dbl (A x1 y1)
    | x1 == x2 && y1 == (-y2) = O
    | otherwise = A x3 y3
    where
      l = (y2 - y1) / (x2 - x1)
      x3 = l * l - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINEABLE add #-}

  char :: Point Weierstrass Affine e q r -> Natural
  char = q_
  {-# INLINABLE char #-}

  cof :: Point Weierstrass Affine e q r -> Natural
  cof = h_
  {-# INLINABLE cof #-}

  dbl :: Point Weierstrass Affine e q r ->
         Point Weierstrass Affine e q r
  dbl O         = O
  dbl (A x y)
    | y == 0    = O
    | otherwise = A x' y'
    where
      a  = a_ (witness :: WAPoint e q r)
      xx = x * x
      l  = (xx + xx + xx + a) / (y + y)
      x' = l * l - x - x
      y' = l * (x - x') - y
  {-# INLINABLE dbl #-}

  def :: Point Weierstrass Affine e q r -> Bool
  def O       = True
  def (A x y) = y * y == (x * x + a) * x + b
    where
      a = a_ (witness :: WAPoint e q r)
      b = b_ (witness :: WAPoint e q r)
  {-# INLINABLE def #-}

  disc :: Point Weierstrass Affine e q r -> q
  disc _ = 4 * a * a * a + 27 * b * b
    where
      a = a_ (witness :: WAPoint e q r)
      b = b_ (witness :: WAPoint e q r)
  {-# INLINABLE disc #-}

  frob :: Point Weierstrass Affine e q r ->
          Point Weierstrass Affine e q r
  frob O       = O
  frob (A x y) = A (F.frob x) (F.frob y)
  {-# INLINABLE frob #-}

  gen :: Point Weierstrass Affine e q r
  gen = gA_
  {-# INLINABLE gen #-}

  id :: Point Weierstrass Affine e q r
  id = O
  {-# INLINABLE id #-}

  inv :: Point Weierstrass Affine e q r ->
         Point Weierstrass Affine e q r
  inv O       = O
  inv (A x y) = A x (-y)
  {-# INLINABLE inv #-}

  order :: Point Weierstrass Affine e q r -> Natural
  order = r_
  {-# INLINABLE order #-}

  