-- |
-- Module      : Primes
-- Description : Números primos y temas relacionados con la teoría de números
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
--      
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Números primos y temas relacionados con la teoría de números, generación de cribas de primos, factorización prima y test Miller-Rabin.
module Math.NumberTheory.Primes
  ( -- * Logaritmo entero
    integerLog2,
    ceilingLog2,

    -- * Raíz cuadrada entera
    isPerfectSquare,
    integerSquareRoot,

    -- * Teoría elemental de números
    divides,
    naiveFactorization',
    isPrimeTrialDivision
  )
where

import Data.Bits ( Bits(shiftR) ) 
import Data.List ( group ) 
import System.Random ( mkStdGen, StdGen ) 
import Control.Monad.Random (runRand, replicateM, MonadRandom (getRandomR))

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- * Teoría de números

-- Logaritmo entero

-- | El entero más grande \(k\) tal que \(2^k\) es menor o igual a \(n\), es decir @intergerLog2 n@ = \(\lfloor \log_2 n \rfloor\)
--
-- Ejemplo:
--
-- >>> integerLog2 13
-- Just 3
integerLog2 :: Integer -> Integer
integerLog2 n
  | n < 0 = error "Entero negativo"
  | otherwise = go n
  where
    go 0 = -1
    go s = 1 + go (shiftR s 1)

-- | El entero más pequeño \(k\) tal que \(2^k\) es mayor o igual a \(n\), es decir @ceilingLog2 n@ = \(\lceil \log_2 n \rceil\)
--
-- Ejemplo:
--
-- >>> ceilingLog2 13
-- Just 4
ceilingLog2 :: Integer -> Integer
ceilingLog2 n
  | n < 0 = error "Entero negativo"
  | otherwise = go n
  where
    go 0 = 0
    go s = 1 + go (shiftR s 1)

--------------------------------------------------------------------------------
-- Raíz cuadrada entera

-- | Calcula la raíz cuadrada entera basandose el método de Herón, y ya que \(\sqrt{n}=2^{\frac{\log_2(n)}{2}}\) es razonable elegir \(n_0=2^{\frac{\lfloor\log_2(n)\rfloor}{2}}).
--
-- Ejemplo:
--
-- >>> integerSquareRoot 125
-- Just 11
integerSquareRoot :: Integer -> Integer
integerSquareRoot n
  | n < 0 = error "Entero negativo"
  | n < 2 = n
  | otherwise = go . (2 ^) . (`div` 2) $ integerLog2 n
  where
    go n0 = if abs (n2 - n1) < 1 then n2 else go n2
      where
        n1 = n0
        a = n `div` n0
        n2 = (n0 + a) `div` 2

-- | Comprueba si un número es un cuadrado perfecto
isPerfectSquare :: Integer -> Bool
isPerfectSquare n = (== 0) . mod n $ integerSquareRoot n

-- | @d `divide a` n@
divides :: Integer -> Integer -> Bool
divides !d !n = mod n d == 0

--------------------------------------------------------------------------------
-- Lista de números primos

-- | Infinita lista de primos, usando la criba de Eratostenes
--
-- Ejemplo :
--
-- >>> take 10 primesEr
-- [2,3,5,7,11,13,17,19,23,29]
primesEr :: [Integer]
primesEr = filterPrime [2 ..]
  where
    filterPrime (p : xs) =
      p : filterPrime [x | x <- xs, x `mod` p /= 0]

------------------------------------------------------------------------- Comprobación de primalidad

-- | Prueba de primalidad ingenua usando la division trivial
--
-- Ejemplo:
--
-- >>> isPrimeTrialDivision 27
-- Just False
isPrimeTrialDivision :: Integer -> Bool
isPrimeTrialDivision n = all (\p -> not (divides p n)) $ ps
  where
    ps = enumFromTo 2 nsqrt
    nsqrt = integerSquareRoot n

-- Test de Miller Rabin

--------------------------------------------------------------------------------
-- Factorización prima

-- | Factorización Ingenua
naiveFactorization :: Integer -> [Integer]
naiveFactorization n
  | n <= 0 = error "Entero negativo"
  | n <= 1 = []
  | otherwise = go n 2
  where
    go k s
      | s * s > k = [k]
      | k `mod` s == 0 = s : go (k `div` s) s
      | otherwise = go k (s + 1)

-- >>> naiveFactorization' 27
-- Just [(3,3)]
naiveFactorization' :: Integer -> [(Integer, Int)]
naiveFactorization' n = map f . group $ naiveFactorization n
  where
    f xs = (head xs, length xs)

-----------------------------------------------------------------------

-- Rápida exponenciación binaria para \(\mathbb{F}_p\).
-- >>> powerMod 3 218 1000
-- 489

powerMod :: Integer -> Integer -> Integer -> Integer
powerMod _ 0 _ = 1
powerMod g a m
  | even a = (x * x) `mod` m
  | otherwise = (g * x * x) `mod` m
  where
    x = powerMod g (a `div` 2) m

decompone :: Integer -> Integer
decompone n
    | even n = 1 + decompone (n `div` 2)
    | otherwise = 0

-- | Testigo de Miller Rabin, devueve True si a es testigo de n

millerRabinWitness :: Integer -> Integer  -> Bool
millerRabinWitness n a
  | even n = False
  | a <= 1 && a>= n-1 = False
  | gcd a n /= 1 = False
  | powerMod a q n /= 1 && all (\x -> x /= (-1)) [powerMod a (2*i*q) n | i<-[0,1..(k-1)]] = True
  | otherwise = False
    where k = decompone (n-1)
          q = (n-1) `div` k

-- >>> millerRabinWitness 172947529 23
-- True

-- | Si encuentra algún testigo es compuesto y devuelve False, de lo contrario probablemente Primo.
-- >>> millerRabinTest 0x100000000000000d9ccec8a39e56f
-- True
millerRabinTest :: Integer -> Bool
millerRabinTest n = not . any (millerRabinWitness n) $ fst (randomIntegerList 10 (1,n-1) (gen 10) )

-- Generador de enteros aleatorios dentro de un rango
randomIntegerList :: Int -> (Integer, Integer) -> StdGen -> ([Integer], StdGen)
randomIntegerList n range = runRand (replicateM n $ getRandomR range)

-- Generador ya determinado
gen :: Int -> StdGen
gen = mkStdGen 
