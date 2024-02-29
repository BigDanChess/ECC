{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Binary
-- Description : Campos binarios
-- Copyright   : (c) Adjoint Inc.
--               (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Definición de campos binarios.
module Math.Algebra.GaloisFields.Binary
  ( Binary,
    BinaryField,
    fromB,
    toB,
    toB',
    toPoly,
  )
where

import Control.Monad.Random (Random (..), RandomGen)
import Data.Bit (Bit, F2Poly, gcdExt, toF2Poly, unF2Poly)
import Data.Bit as V 
import Data.Euclidean as S (Euclidean (..), GcdDomain)
import Data.Field (Field)
import Data.Group (Group (..))
import Data.Semiring (Ring (..), Semiring (..))
import Data.Vector.Unboxed as V (fromList, length, toList)
import GHC.Exts (IsList (..))
import GHC.TypeNats (Natural, natVal)
import Math.Algebra.GaloisFields.Base (GaloisField (..))
import Protolude as P hiding (Semiring, natVal, show, writeFile)
import Test.QuickCheck (Arbitrary (..), Gen, choose)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty (..))
import Math.NumberTheory.ArithmeticFunctions (moebiusMu)

-- | Clase para campos binarios (con orden de la forma \(2^q\))
class (GaloisField k) => BinaryField k where
  {-# MINIMAL fromB #-}

  -- | Convierte un elemento de \(\mathbb{F}_{2^q}[X]/\langle f(X) \rangle\) a uno de \(\mathbb{Z}\).
  fromB :: k -> Integer

{-
Antes
newtype Binary (p :: Nat) = B F2Poly
  deriving (Eq, Generic, NFData, Ord, Show)
-}

-- | Elementos del campo binario
newtype Binary :: Nat -> Type where 
  B :: F2Poly -> Binary p deriving (Eq, Generic, NFData, Ord, Show)

-- | Los campos binarios son campos de Galois.
instance (KnownNat p) => GaloisField (Binary p) where
  char :: Binary p -> Natural
  char = const 2
  {-# INLINEABLE char #-}
  deg :: Binary p -> Word
  deg = pred . fromIntegral . V.length . unF2Poly . toPoly . natVal
  {-# INLINEABLE deg #-}
  frob :: Binary p -> Binary p
  frob = join (*)
  {-# INLINEABLE frob #-}

{-# RULES "Binary.pow" forall (k :: (KnownNat p) => Binary p) n. (^) k n = pow k n #-}

-- | Los campos binarios son convertibles
instance (KnownNat p) => BinaryField (Binary p) where
  fromB :: Binary p -> Integer
  fromB (B x) = toInteger x
  {-# INLINEABLE fromB #-}

-- * Instancias de Grupo

-- | Los campos binarios son semigrupos multiplicativos
instance (KnownNat p) => Semigroup (Binary p) where
  (<>) :: Binary p -> Binary p -> Binary p
  (<>) = (*)
  {-# INLINE (<>) #-}
  stimes :: (Integral b) => b -> Binary p -> Binary p
  stimes = flip pow
  {-# INLINE stimes #-}

-- | Los campos binarios son monoides multiplicativos
instance (KnownNat p) => Monoid (Binary p) where
  mempty :: Binary p
  mempty = B 1
  {-# INLINE mempty #-}

-- | Los campos binarios son grupos multiplicativos
instance (KnownNat p) => Group (Binary p) where
  invert :: Binary p -> Binary p
  invert = recip
  {-# INLINE invert #-}
  pow :: (Integral x) => Binary p -> x -> Binary p
  pow x n
    | n >= 0 = x ^ n
    | otherwise = recip x ^ P.negate n
  {-# INLINEABLE pow #-}

-- * Instancias numéricas

-- | Los campos binarios son numéricos
instance (KnownNat p) => Num (Binary p) where
  -- | Suma de polinomios con reducción módulo 2 en los coeficientes.
  (+) :: Binary p -> Binary p -> Binary p
  B x + B y = B $ x + y
  {-# INLINE (+) #-}
  -- | Multiplicación de polinomios módulo el polinomio irreducible
  (*) :: Binary p -> Binary p -> Binary p
  B x * B y = B $ P.rem (x * y) pIrr
    where 
      pIrr = toPoly $ natVal $ witness @(Binary p)
  {-# INLINE (*) #-}
  (-) :: Binary p -> Binary p -> Binary p
  B x - B y = B $ x + y
  negate :: Binary p -> Binary p
  {-# INLINE (-) #-}
  negate = identity
  {-# INLINE negate #-}
  fromInteger :: Integer -> Binary p
  fromInteger = B . flip P.rem pIrr . toPoly
    where 
      pIrr = toPoly $ natVal $ witness @(Binary p)
  {-# INLINEABLE fromInteger #-}
  abs :: Binary p -> Binary p
  abs = panic "Binary.abs: no implementado."
  signum :: Binary p -> Binary p
  signum = panic "Binary.signum: no implementado."

-- | Los campos binarios son racionales.
instance (KnownNat p) => Fractional (Binary p) where
  recip :: Binary p -> Binary p
  recip (B x) =
    case gcdExt x pIrr of
      (1, y) -> B y
      _ -> divZeroError
    where
      pIrr = toPoly $ natVal (witness :: Binary p)
  {-# INLINE recip #-}
  fromRational :: Rational -> Binary p
  fromRational rat = fromInteger (numerator rat) / fromInteger (denominator rat)
  {-# INLINEABLE fromRational #-}


-- * Instancias de semianillo

-- | Los campos binarios son semianillos
instance (KnownNat p) => Semiring (Binary p) where
  fromNatural :: Natural -> Binary p
  fromNatural = fromIntegral
  {-# INLINEABLE fromNatural #-}
  one :: Binary p
  one = B 1
  {-# INLINE one #-}
  plus :: Binary p -> Binary p -> Binary p
  plus = (+)
  {-# INLINE plus #-}
  times :: Binary p -> Binary p -> Binary p
  times = (*)
  {-# INLINE times #-}
  zero :: Binary p
  zero = B 0
  {-# INLINE zero #-}

-- | Los campos binarios son anillos
instance (KnownNat p) => Ring (Binary p) where
  negate :: Binary p -> Binary p
  negate = P.negate
  {-# INLINE negate #-}

-- | Los campos binarios son dominios euclideanos
instance (KnownNat p) => Euclidean (Binary p) where
  degree :: Binary p -> Natural
  degree (B x) = pred . fromIntegral . V.length . unF2Poly $ x
  quotRem :: Binary p -> Binary p -> (Binary p, Binary p)
  quotRem (B x) (B y) = (B x',B y')
    where (x',y') = P.quotRem x y
  {-# INLINE quotRem #-}

-- | Los campos binarios es un dominio de máximo común divisor
instance (KnownNat p) => GcdDomain (Binary p)

-- | Los campos binarios son campos
instance (KnownNat p) => Field (Binary p)


-- | Otras instancias

-- | Los campos binarios son aleatorios.
instance (KnownNat p) => Random (Binary p) where
  random :: (RandomGen g) => g -> (Binary p, g)
  random = randomR (B 0, B $ toPoly $ order pIrr - 1)
    where pIrr = witness @(Binary p)
  {-# INLINEABLE random #-}
  randomR :: (RandomGen g) => (Binary p, Binary p) -> g -> (Binary p, g)
  randomR (a, b) = first toB' . randomR (fromB a, fromB b)
  {-# INLINEABLE randomR #-}


-- | Los campos binarios son listas
instance (KnownNat p) => IsList (Binary p) where
  type Item (Binary p) = Bit
  fromList :: [Item (Binary p)] -> Binary p
  fromList = B . toF2Poly . V.fromList
  {-# INLINEABLE fromList #-}
  toList :: Binary p -> [Item (Binary p)]
  toList (B x) = V.toList $ unF2Poly x
  {-# INLINEABLE toList #-}


-- | Los campos binarios están acotados
instance (KnownNat p) => Bounded (Binary p) where
  maxBound :: Binary p
  maxBound = B $ toPoly $ order pIrr - 1
    where pIrr = witness @(Binary p)
  {-# INLINE maxBound #-}
  minBound :: Binary p
  minBound = B 0
  {-# INLINE minBound #-}

-- | Los campos binarios son enumerables
instance (KnownNat p) => Enum (Binary p) where
  fromEnum :: Binary p -> Int
  fromEnum = fromIntegral
  {-# INLINEABLE fromEnum #-}
  toEnum :: Int -> Binary p
  toEnum = fromIntegral
  {-# INLINEABLE toEnum #-}

-- | Los campos binarios son reales
instance (KnownNat p) => Real (Binary p) where
  toRational :: Binary p -> Rational
  toRational = fromIntegral
  {-# INLINEABLE toRational #-}

-- | Los campos binarios son integrales
instance (KnownNat p) => Integral (Binary p) where
  quotRem :: Binary p -> Binary p -> (Binary p, Binary p)
  quotRem = S.quotRem
  {-# INLINE quotRem #-}
  toInteger :: Binary p -> Integer
  toInteger = fromB
  {-# INLINEABLE toInteger #-}

-- | Conversión segura de \(\mathbb{Z}\) a \(\mathbb{F}_{2^q}[X]/\langle f(X) \rangle \).
toB :: (KnownNat p) => Integer -> Binary p
toB = fromInteger
{-# INLINEABLE toB #-}

-- | Conversión insegura de \(\mathbb{Z}\) a \(\mathbb{F}_{2^q}[X \langle f(X) \rangle \).
toB' :: Integer -> Binary p
toB' = B . toPoly
{-# INLINEABLE toB' #-}

-- | Especificación para convertir de entero a polinomio.
--
-- Ejemplo:
--
-- >>> toPoly 5
-- 0b101
toPoly :: (Integral a) => a -> F2Poly
toPoly = fromIntegral
{-# INLINEABLE toPoly #-}
{-# SPECIALIZE toPoly :: Integer -> F2Poly #-}
{-# SPECIALIZE toPoly :: Natural -> F2Poly #-}

