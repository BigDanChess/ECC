-- |
-- Module      : SECT11ER1
-- Description : Curva SECT113R1
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Instancia de curva elíptica SECT113R1.
module Math.EllipticCurve.Curve.Binary.SECT113R1
  ( module Math.EllipticCurve.Curve.Binary,
    Point (..),
    module Math.EllipticCurve.Curve.Binary.SECT113R1,
  )
where

import GHC.Natural (Natural)
import Math.Algebra.Galois
import Math.EllipticCurve.Curve.Binary
import Protolude
import Text.PrettyPrint.Leijen.Text (Pretty(pretty))


-- * Tipos

-- | Curva SECT113R1.
data SECT113R1

-- | Campo de puntos de la curva SECT113R1.
type F2n = Binary P

-- | Polinomio irreducible \(f(x)=x^113+x^9 +1\) sobre \(\mathbb{F}_2[X]\)
type P = 0x20000000000000000000000000201

-- >>> :k F2m
-- F2m :: *

-- | Campo de coeficientes de la curva SECT113R1.
type Fr = Prime R

-- | Número primo 5192296858534827689835882578830703
type R = 0x100000000000000d9ccec8a39e56f

-- | La curva SECT113R1 es una curva binaria.
instance (Curve 'Binary c SECT113R1 F2n Fr) => BCurve c SECT113R1 F2n Fr where
  a_ :: BPoint c SECT113R1 F2n Fr -> F2n
  a_ = const _a
  {-# INLINEABLE a_ #-}
  b_ :: BPoint c SECT113R1 F2n Fr -> F2n
  b_ = const _b
  {-# INLINEABLE b_ #-}
  h_ :: BPoint c SECT113R1 F2n Fr -> Natural
  h_ = const _h
  {-# INLINEABLE h_ #-}
  p_ :: BPoint c SECT113R1 F2n Fr -> Natural
  p_ = const _p
  {-# INLINEABLE p_ #-}
  r_ :: BPoint c SECT113R1 F2n Fr -> Natural
  r_ = const _r
  {-# INLINEABLE r_ #-}

-- \| Punto afín de la curva SECT113R1.
type PA = BAPoint SECT113R1 F2n Fr

-- | La curva afín SECT113R1 es una curva afín binaria.
instance BACurve SECT113R1 F2n Fr where
  gA_ :: BAPoint SECT113R1 F2n Fr
  gA_ = gA
  {-# INLINEABLE gA_ #-}



-- * Parámetros

-- | Coeficiente \(A\) de la curva SECT113R1.
_a :: F2n
_a = 0x3088250CA6E7C7FE649CE85820F7
{-# INLINEABLE _a #-}

-- | Coeficiente \(B\) de la curva SECT113R1.
_b :: F2n
_b = 0xe8bee4d3e2260744188be0e9c723
{-# INLINEABLE _b #-}


-- | Cofactor de la curva SECT113R1.
_h :: Natural
_h = 0x2
{-# INLINEABLE _h #-}

-- | Polinomio de la curva SECT113R1.
_p :: Natural
_p = 0b100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000001
{-# INLINEABLE _p #-}

-- | Orden de la curva SECT113R1.
_r :: Natural
_r = 0x100000000000000d9ccec8a39e56f
{-# INLINEABLE _r #-}

-- | Coordenada \(x\) de la curva SECT113R1.
_x :: F2n
_x = 0x9d73616f35f4ab1407d73562c10f
{-# INLINEABLE _x #-}

-- | Coordinate \(Y\) de la curva SECT113R1.
_y :: F2n
_y = 0xa52830277958ee84d1315ed31886
{-# INLINEABLE _y #-}

-- | Generador de la curva afín SECT113R1.
gA :: PA
gA = A _x _y
{-# INLINEABLE gA #-}

