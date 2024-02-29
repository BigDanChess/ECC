{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Prime
-- Description : Campos primos
-- Copyright   : (c) Adjoint Inc.
--               (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definición de campos primos.

module Math.Algebra.GaloisFields.Prime
  ( Prime,
    PrimeField,
    fromP,
    toP,
  )
where

import Control.Monad.Random (Random(..),RandomGen)
import Test.QuickCheck (Arbitrary(..), choose, Gen)
import Data.Euclidean as S (Euclidean (..), GcdDomain)
import Data.Field (Field)
import Data.Group (Group (..))
import Data.Mod (Mod, unMod, (^%))
import Data.Semiring (Ring (..), Semiring (..))
import GHC.Natural (naturalToInteger)
import GHC.TypeNats (natVal,Natural)
import Math.Algebra.GaloisFields.Base (GaloisField (..))
import Protolude as P hiding (Semiring, natVal, rem,show, writeFile)
import Text.PrettyPrint.Leijen.Text (Pretty (..),Doc)
import Math.NumberTheory.ArithmeticFunctions (moebiusMu,divisors)


-- * Tipo de dato


-- | Campos primos \(\mathbb{F}_p = \mathbb{Z}/p\mathbb{Z}\) para \(p\) primo.
class (GaloisField k) => PrimeField k where
  {-# MINIMAL fromP #-}

  -- | Convertir de \(\mathbb{F}_p\) a \(\mathbb{Z}\).
  fromP :: k -> Integer

-- | Elementos del campo primo.
newtype Prime (p :: Nat) = P (Mod p)
  deriving (Eq, Ord, Show, Generic, Num, Fractional, Euclidean, Field, GcdDomain, Ring, Semiring, Bounded, Enum, NFData)

instance Hashable (Prime p) where
  hashWithSalt :: Int -> Prime p -> Int
  hashWithSalt s (P x) = hashWithSalt s (unMod x)

-- | Los campos primos son campos de Galois.
instance (KnownNat p) => GaloisField (Prime p) where
  char :: Prime p -> Natural
  char = natVal
  {-# INLINEABLE char #-}
  deg :: Prime p -> Word
  deg = const 1
  {-# INLINEABLE deg #-}
  frob :: Prime p -> Prime p
  frob = identity
  {-# INLINEABLE frob #-}

{-# RULES
"Prime.pow" forall (k :: (KnownNat p) => Prime p) n.
  (^) k n =
    pow k n
  #-}

-- | Los campos primos son convertibles.
instance (KnownNat p) => PrimeField (Prime p) where
  fromP :: Prime p -> Integer
  fromP (P x) = naturalToInteger (unMod x)
  {-# INLINEABLE fromP #-}


-- * Instancias de grupo

-- | Los campos primos son semigrupos multiplicativos.
instance (KnownNat p) => Semigroup (Prime p) where
  (<>) :: Prime p -> Prime p -> Prime p
  (<>) = (*)
  {-# INLINE (<>) #-}
  stimes :: (Integral b) => b -> Prime p -> Prime p
  stimes = flip pow
  {-# INLINE stimes #-}

-- | Los campos primos son monoides multiplicativos
instance (KnownNat p) => Monoid (Prime p) where
  mempty :: Prime p
  mempty = P 1
  {-# INLINE mempty #-}

-- | Los campos primos son grupos multiplicativos
instance (KnownNat p) => Group (Prime p) where
  invert :: Prime p -> Prime p
  invert = recip
  {-# INLINE invert #-}
  pow :: (Integral x) => Prime p -> x -> Prime p
  pow (P x) k = P (x ^% k)
  {-# INLINE pow #-}


-- * Otras instancias

-- | Los campos primos son aleatorios.
instance KnownNat p => Random (Prime p) where
  random :: (RandomGen g) => g -> (Prime p, g)
  random         = randomR (minBound, maxBound)
  {-# INLINABLE random #-}
  randomR :: (RandomGen g) => (Prime p, Prime p) -> g -> (Prime p, g)
  randomR (a, b) = first fromInteger . randomR (fromP a, fromP b)
  {-# INLINABLE randomR #-}

-- | Los campos primos son Real.
instance (KnownNat p) => Real (Prime p) where
  toRational :: Prime p -> Rational
  toRational = fromIntegral
  {-# INLINEABLE toRational #-}

-- | Los campos primos son integrales.
instance (KnownNat p) => Integral (Prime p) where
  quotRem :: Prime p -> Prime p -> (Prime p, Prime p)
  quotRem = S.quotRem
  {-# INLINE quotRem #-}
  toInteger :: Prime p -> Integer
  toInteger = fromP
  {-# INLINEABLE toInteger #-}

-------------------------------------------------------------------------------
-- | Funciones auxiliares
-------------------------------------------------------------------------------

-- | Conversión segura de \(\mathbb{Z}\) a \(\mathbb{F}_p\).
toP :: (KnownNat p) => Integer -> Prime p
toP = fromInteger
{-# INLINEABLE toP #-}

numberOfIrreduciblePolynomials :: (KnownNat p) => Prime p -> Integer-> Integer 
numberOfIrreduciblePolynomials p n = sum [moebiusMu (n `div` d) * toInteger (p'^d) | d<-divs] `div` n
  where divs = divisors n 
        p' = char p 