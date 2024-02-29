{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Binary2
-- Description : Curvas sobre campos binarios
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definición de curvas elípticas sobre campos binarios con a_1=0.
module Math.EllipticCurve.Curve.Binary2 where

import Control.Monad.Random (Random)
import Data.Group (Group)
import GHC.Natural (Natural)
import Math.Algebra.Galois as F (GaloisField, PrimeField, frob)
import Math.EllipticCurve.Curve
import Protolude
import Test.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty (..))

-- | Puntos binarios
type B2Point = Point 'Binary2

-- | Curvas binarias.
class
  (GaloisField q, PrimeField r, Curve 'Binary2 c e q r) =>
  B2Curve c e q r
  where
  {-# MINIMAL a_, b_, c_, h_, p_, r_ #-}
  a_ ::
    B2Point c e q r ->
    -- | Coeficiente @A@.
    q
  b_ ::
    B2Point c e q r ->
    -- | Coeficiente @B@.
    q
  c_ ::
    B2Point c e q r ->
    -- | Coeficiente @C@
    q
  h_ ::
    B2Point c e q r ->
    -- | Cofactor de Curva.
    Natural
  p_ ::
    B2Point c e q r ->
    -- | Polinomio de curva.
    Natural
  r_ ::
    B2Point c e q r ->
    -- | Orden de curva.
    Natural

-- *  Coordenadas afines

-- | Puntos afines binarios.
type B2APoint = B2Point 'Affine

-- | Curvas afines binarias \(y^2 + cy = x^3 + ax + b\).
class (B2Curve 'Affine e q r) => B2ACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ ::
    -- | Generador de curva.
    B2APoint e q r

-- | Las curvas afines binarias 2 son curvas elípticas.
instance (B2ACurve e q r) => Curve 'Binary2 'Affine e q r where
  data Point 'Binary2 'Affine e q r
    = A q q
    | -- \^ Punto afín.
      O
    -- \^ Punto en el infinito.
    deriving (Eq, Generic, NFData, Read, Show)

  add ::
    Point 'Binary2 'Affine e q r ->
    Point 'Binary2 'Affine e q r ->
    Point 'Binary2 'Affine e q r
  add p O = p
  add O q = q
  add p@(A x1 y1) q@(A x2 y2)
    | xx == 0 && yy + c == 0 = O -- Si Q es la inversa de P
    | x1 == x2 = dbl p
    | otherwise = A x3 y3
    where
      c = c_ (witness :: B2APoint e q r)
      xx = x1 + x2
      yy = y1 + y2
      l = yy / xx
      x3 = l * l + xx
      y3 = l * (x1 + x3) + y1 + c
  {-# INLINEABLE add #-}

  char :: Point Binary2 Affine e q r -> Natural
  char = const 2
  {-# INLINEABLE char #-}

  cof :: Point Binary2 Affine e q r -> Natural
  cof = h_
  {-# INLINEABLE cof #-}

  dbl :: Point Binary2 Affine e q r -> Point Binary2 Affine e q r
  dbl O = O
  dbl (A x y) = A x' y'
    where
      a = a_ (witness :: B2APoint e q r)
      c = c_ (witness :: B2APoint e q r)
      v = (x * x + a) / c
      x' = v * v
      y' = v * (x + x') + y + c
  {-# INLINEABLE dbl #-}

  def :: Point Binary2 Affine e q r -> Bool
  def O = True
  def (A x y) = (x * x + a) * x + b + y * y + c * y == 0
    where
      a = a_ (witness :: B2APoint e q r)
      b = b_ (witness :: B2APoint e q r)
      c = c_ (witness :: B2APoint e q r)
  {-# INLINEABLE def #-}

  disc :: Point Binary2 Affine e q r -> q
  disc _ = c^4 
    where 
        c = c_ (witness :: B2APoint e q r)
  {-# INLINEABLE disc #-}

  frob :: Point Binary2 Affine e q r -> Point Binary2 Affine e q r
  frob O = O
  frob (A x y) = A (F.frob x) (F.frob y)
  {-# INLINEABLE frob #-}

  gen :: Point Binary2 Affine e q r
  gen = gA_
  {-# INLINEABLE gen #-}

  id :: Point Binary2 Affine e q r
  id = O
  {-# INLINEABLE id #-}

  inv :: Point Binary2 Affine e q r -> Point Binary2 Affine e q r
  inv O = O
  inv (A x y) = A x (y + c)
    where
      c = c_ (witness :: B2APoint e q r)
  {-# INLINEABLE inv #-}

  order :: Point Binary2 Affine e q r -> Natural
  order = r_
  {-# INLINEABLE order #-}
