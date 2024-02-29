-- |
-- Module      : SECT11ER1
-- Description : Curva SECT11ER1
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Instancia de curva elíptica SECT11ER1.
module Math.EllipticCurve.Curve.Weierstrass.SECP112R1
  ( module Math.EllipticCurve.Curve.Weierstrass,
    Point (..),
    module Math.EllipticCurve.Curve.Weierstrass.SECP112R1,
  )
where

import GHC.Natural (Natural)
import Math.Algebra.Galois
import Math.EllipticCurve.Curve
import Math.EllipticCurve.Curve.Weierstrass
import Protolude
import Text.PrettyPrint.Leijen.Text (Pretty (pretty))

-------------------------------------------------------------------------------
-- Tipos
-------------------------------------------------------------------------------

-- | Curva SECP112R1.
data SECP112R1

-- | Campo de puntos de la curva SECP112R1.
type Fq = Prime Q

type Q = 0xdb7c2abf62e35e668076bead208b

-- | Campo de coeficientes de la curva SECP112R1.
type Fr = Prime R

type R = 0xdb7c2abf62e35e7628dfac6561c5

-- | Punto afín de la curva SECP112R1 .
type PA = WAPoint SECP112R1 Fq Fr

-------------------------------------------------------------------------------
-- Parámetros
-------------------------------------------------------------------------------

-- | Coeficiente @A@ de la curva SECP112R1.
_a :: Fq
_a = 0xdb7c2abf62e35e668076bead2088
{-# INLINEABLE _a #-}

-- | Coeficiente @B@ de la curva SECP112R1.
_b :: Fq
_b = 0x659ef8ba043916eede8911702b22
{-# INLINEABLE _b #-}

-- | Cofactor de la curva SECP112R1.
_h :: Natural
_h = 0x1
{-# INLINEABLE _h #-}

-- | Característica de la curva SECP112R1.
_q :: Natural
_q = 0xdb7c2abf62e35e668076bead208b
{-# INLINEABLE _q #-}

-- | Orden de la curva SECP112R1.
_r :: Natural
_r = 0xdb7c2abf62e35e7628dfac6561c5
{-# INLINEABLE _r #-}

-- | Coordenada @X@ de la curva SECP112R1.
_x :: Fq
_x = 0x9487239995a5ee76b55f9c2f098
{-# INLINEABLE _x #-}

-- | Coordenada @Y@ de la curva SECP112R1.
_y :: Fq
_y = 0xa89ce5af8724c0a23e0e0ff77500
{-# INLINEABLE _y #-}

-- | Generator of affine SECP112R1 curve.
gA :: PA
gA = A _x _y
{-# INLINEABLE gA #-}

-- La curva SECP112R1 es una curva de Weierstrass.
instance (Curve 'Weierstrass c SECP112R1 Fq Fr) => WCurve c SECP112R1 Fq Fr where
  a_ :: WPoint c SECP112R1 Fq Fr -> Fq
  a_ = const _a
  {-# INLINEABLE a_ #-}
  b_ :: WPoint c SECP112R1 Fq Fr -> Fq
  b_ = const _b
  {-# INLINEABLE b_ #-}
  h_ :: WPoint c SECP112R1 Fq Fr -> Natural
  h_ = const _h
  {-# INLINEABLE h_ #-}
  q_ :: WPoint c SECP112R1 Fq Fr -> Natural
  q_ = const _q
  {-# INLINEABLE q_ #-}
  r_ :: WPoint c SECP112R1 Fq Fr -> Natural
  r_ = const _r
  {-# INLINEABLE r_ #-}

-- La curva afín SECP112R1 es una curva afín de Weierstrass.
instance WACurve SECP112R1 Fq Fr where
  gA_ :: WAPoint SECP112R1 Fq Fr
  gA_ = gA
  {-# INLINEABLE gA_ #-}
