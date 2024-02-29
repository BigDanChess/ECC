-- |
-- Module      : ArithmeticFunctions
-- Description : Funciones aritméticas
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Funciones aritméticas.
module Math.NumberTheory.ArithmeticFunctions (
    moebiusMu,
    eulerTotientNavie,
    eulerTotient2,
    eulerTotient3,
    liouville,
    convolutionDirichlet,
    divisors
) where

import Math.NumberTheory.Primes

-- | La función \(\mu\) de Moebius (implementación ingenua).
-- Se define como:
-- \[ \mu(1) = 1 ; \]
--
-- Si \(n>1\), se escribe \(n=p_1^{\alpha_1}\dotsc p_k^{\alpha_k}\). Entonces
-- \[ \begin{aligned} &\mu(n) = (-1)^k \text{ si } a_1=a_2=\dotsc=a_k=1, \\ & \mu(n) = 0 \text{ en otro caso } \end{aligned}\]
-- Ejemplo:
--
-- >>> moebiusMu 19
-- -1
moebiusMu :: Integer -> Integer
moebiusMu n
  | n <= 0 = error "Entero negativo"
  | n == 1 = 1
  | otherwise = if all (== 1) [snd x | x <- ps] then (-1) ^ k else 0
  where
    ps = naiveFactorization' n
    k = length ps

-- | Función \(\varphi\) de Euler (o función totiente)  implemtación ingenua
--
-- \[\varphi(n)=\sum_{k=1}^n 1,\]
--
-- donde \('\) indica que la suma se extiende sobre aquellos \(k\) que son coprimos con \(n\).
--
-- Ejemplo:
--
-- >>> eulerTotientNavie 10
-- 4
eulerTotientNavie :: Integer -> Int
eulerTotientNavie n
  | n <= 0 = error "Entero negativo"
  | otherwise = length [s | s <- [1 .. n], gcd s n == 1]

divisors :: Integer -> [Integer]
divisors n
  | n <= 0 = error "Entero negativo"
  | otherwise = [s | s <- [1 .. n], s `divides` n]

-- >>> eulerTotient2 10
-- 4

eulerTotient2 :: Integer -> Integer
eulerTotient2 n
  | n <= 0 = error "Entero negativo"
  | otherwise = sum $ map (\d -> moebiusMu d * (n `div` d)) (divisors n)

-- >>> eulerTotient3 10
-- 4

eulerTotient3 :: Integer -> Integer
eulerTotient3 n
  | n <= 0 = error "Entero no positivo"
  | otherwise = n * num `div` de
  where
    primes = [fst t | t <- naiveFactorization' n]
    de = product primes
    num = product $ (\x -> x - 1) <$> primes

liouville :: Integer -> Integer 
liouville n 
 | n<=0 = error "Entero no positivo"
 | n == 1 = 1 
 | otherwise = (-1)^k 
    where k = sum [snd x | x <- ps] 
          ps = naiveFactorization' n 

convolutionDirichlet :: (Integer -> Integer) -> (Integer -> Integer) -> Integer -> Integer 
convolutionDirichlet f g n
    | n<=0 = error "Entero no positivo"
    | otherwise = sum [f s * g(n `div` s) | s<- divisors n]
