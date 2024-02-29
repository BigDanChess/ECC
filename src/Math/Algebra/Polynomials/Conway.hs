{-# LANGUAGE UndecidableInstances,AllowAmbiguousTypes, TemplateHaskell #-}
-- |
-- Module      : Conway
-- Description : Lista de polinomios de conway para campos binarios
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
--      
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Lista de Polinomios de Conway para campos binarios, la información se extrajo de https://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html
module Math.Algebra.Polynomials.Conway where

import GHC.TypeLits
import Math.Algebra.GaloisFields.Binary ( Binary )
import Math.Algebra.GaloisFields.Prime ( Prime)
import Math.NumberTheory.Primes

import Data.Bit (Bit, F2Poly, gcdExt, toF2Poly, unF2Poly)

import GHC.TypeLits
import Data.Typeable
import Data.Kind (Type)
import Data.Data (Proxy(Proxy))

import qualified Language.Haskell.TH as TH 

-- | Lista de polinomios de Conway para campos Binarios, de la forma \([n,f(x)]\) con \(n\) el grado del polinomio irreducible \(f(x)\).
conwayBinaryPoly :: [[Natural]]
conwayBinaryPoly =
  [ [1, 0b11],
    [2, 0b111],
    [3, 0b1101],
    [4, 0b11001],
    [5, 0b101001],
    [6, 0b1101101],
    [7, 0b11000001],
    [8, 0b101110001],
    [9, 0b1000100001],
    [10, 0b11110110001],
    [11, 0b101000000001],
    [12, 0b1101011100001],
    [13, 0b11011000000001],
    [14, 0b100101010000001],
    [15, 0b1010110000000001],
    [16, 0b10110100000000001],
    [17, 0b100100000000000001],
    [18, 0b1100000000101000001],
    [19, 0b11100100000000000001],
    [20, 0b110011110110000000001],
    [21, 0b1010011000000000000001],
    [22, 0b10000110111110000000001],
    [23, 0b100001000000000000000001],
    [24, 0b1001010101100111100000001],
    [25, 0b10100010100000000000000001],
    [26, 0b110010111010001000000000001],
    [27, 0b1011010101101000000000000001],
    [28, 0b10100111000001000000000000001],
    [29, 0b101000000000000000000000000001],
    [30, 0b1111010100010100110000000000001],
    [31, 0b10010000000000000000000000000001],
    [32, 0b100110010100000100000000000000001],
    [33, 0b1001001010111100000000000000000001],
    [34, 0b11101111100110011000000000000000001],
    [35, 0b101001010011000000000000000000000001],
    [36, 0b1100011010000110010110110000000000001],
    [37, 0b11111100000000000000000000000000000001],
    [38, 0b111001001110001000000000000000000000001],
    [39, 0b1010011101111001000000000000000000000001],
    [40, 0b11010100100011011010010100000000000000001],
    [41, 0b100100000000000000000000000000000000000001],
    [42, 0b1110011001011000001010001110001000000000001],
    [43, 0b10011010000000000000000000000000000000000001],
    [44, 0b110110000000000011010000100000000000000000001],
    [45, 0b1000001000011011010010000000000000000000000001],
    [46, 0b10000000000000100100110100000000000000000000001],
    [47, 0b100001000000000000000000000000000000000000000001],
    [48, 0b1001000110111000010000010100000000000000000000001],
    [49, 0b11111010101000000000000000000000000000000000000001],
    [50, 0b101010101110111011010000000111000000000000000000001],
    [51, 0b1000001001001001100000000000000000000000000000000001],
    [52, 0b11001001001000110100010101111000000000000000000000001],
    [53, 0b111000100000000000000000000000000000000000000000000001],
    [54, 0b1110100100000101111001000101011110100000000000000000001],
    [55, 0b10001001011100000000000000000000000000000000000000000001],
    [56, 0b101110001101011000010010001000100100000000000000000000001],
    [57, 0b1111111010110100100101000000000000000000000000000000000001],
    [58, 0b11010111101110001010001011100101000000000000000000000000001],
    [59, 0b110111100000000000000000000000000000000000000000000000000001],
    [60, 0b1011110010001000010100100110001011101001011011000000000000001],
    [61, 0b11100100000000000000000000000000000000000000000000000000000001],
    [62, 0b110000100000111011111100111111101000000000000000000000000000001],
    [63, 0b1111100011010001110000111000000000000000000000000000000000000001],
    [64, 0b11101101001111000010111111100010010000000000000000000000000000001],
    [65, 0b110001010110111100000000000000000000000000000000000000000000000001],
    [66, 0b1010111100010100100101111001011111100010101001100000000000000000001],
    [67, 0b11100100000000000000000000000000000000000000000000000000000000000001],
    [68, 0b110011001100011100001010001011100001000000000000000000000000000000001],
    [69, 0b1000110011100010011000001000000000000000000000000000000000000000000001],
    [70, 0b10110001111111110001110010100110111111010001100000000000000000000000001],
    [71, 0b110101000000000000000000000000000000000000000000000000000000000000000001],
    [72, 0b1000100010001001001001110101011110010010000010011000000000000000000000001],
    [73, 0b10111000000000000000000000000000000000000000000000000000000000000000000001],
    [74, 0b100100001001110011000100101111111111110000000000000000000000000000000000001],
    [75, 0b1100110001001010000000110100010010000000000000000000000000000000000000000001],
    [76, 0b11100100000000110001100111010101011111100000000000000000000000000000000000001],
    [77, 0b101010000001001100000000000000000000000000000000000000000000000000000000000001],
    [78, 0b1100101111010111011101101101100011110101001110100000000000000000000000000000001],
    [79, 0b10111000000000000000000000000000000000000000000000000000000000000000000000000001],
    [80, 0b101011101101010101001110111001011100001001100010100000000000000000000000000000001],
    [81, 0b1001101101110001110001011011000000000000000000000000000000000000000000000000000001],
    [82, 0b11111011010111011100011010001000111111000000000000000000000000000000000000000000001],
    [83, 0b101010010000000000000000000000000000000000000000000000000000000000000000000000000001],
    [84, 0b1010110111011100110011001110100110000101010000111000100001000000000000000000000000001],
    [85, 0b10100011000000111010101000000000000000000000000000000000000000000000000000000000000001],
    [86, 0b111001111110101010000110010100010000001010010000000000000000000000000000000000000000001],
    [87, 0b1101010110111110100010000010101000000000000000000000000000000000000000000000000000000001],
    [88, 0b11011101110111110011010010011011010010100100110100000000000000000000000000000000000000001],
    [89, 0b100101100000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [90, 0b1010100100111010101000001110101010101111101011111011010111101111100000000000000000000000001],
    [91, 0b11001000011011011100000000000000000000000000000000000000000000000000000000000000000000000001],
    [92, 0b101011000100101111100111100000101100011011111000100000000000000000000000000000000000000000001],
    [95, 0b110100101100111011100100100000000000000000000000000000000000000000000000000000000000000000000001],
    [96, 0b1011101111010111101111011000100011000100100010100010001000011101000000000000000000000000000000001],
    [97, 0b10000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [98, 0b110100101011001100100010001110010000000100001010101101101000000000000000000000000000000000000000001],
    [100, 0b10010110110100011001101011010011001111000101111110001001110000000000000000000000000000000000000000001],
    [101, 0b110000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [102, 0b1000000000100000100000011101101000000101010011010101101111110100000010000000000000000000000000000000001],
    [103, 0b10111101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [107, 0b111101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [108, 0b1011000111110001001010011010011111110110001111110010001001010111000010100000000000000000000000000000000000001],
    [109, 0b10101100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [110, 0b111000100011011001001000011011000101110100110000100000111111001011001000000000000000000000000000000000000000001],
    [113, 0b101101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [114, 0b1001000101010101101011000111101100111100101000010010101111001011110011110101010000000000000000000000000000000000001],
    [115, 0b11001011011111110110001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [119, 0b111000010000011111101100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [120, 0b1011110100001110110000000100111100101010111011111011100010101000010110010101000101010110000000000000000000000000000000001],
    [121, 0b10100011001111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [125, 0b100101100111110111010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [126, 0b1100011010110101110110101100100000001101111111010001100011011010100010110010010101000111010000000000000000000000000000000000001],
    [127, 0b11000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [131, 0b110011110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [132, 0b1000110101100000101010011011001000010111010110000011100001001001010001110000010010001100110100000000000000000000000000000000000000001],
    [133, 0b10000110100101011101111001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [137, 0b101111001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [139, 0b11110101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [143, 0b111010010001101011000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [149, 0b110111110100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [150, 0b1101011010001010101010100111100100111000010001010011111101111010111100000101100111100101101111000011110101101011000000000000000000000000000000000000001],
    [151, 0b10010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [157, 0b10100110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [163, 0b10010011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [167, 0b100000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [169, 0b10010110110010101100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [173, 0b101001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [179, 0b111010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [181, 0b11000011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [191, 0b110111010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [193, 0b11101111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [197, 0b111101111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [199, 0b10110111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [211, 0b11010110010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [223, 0b10101100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [227, 0b110111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [229, 0b11100110010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [233, 0b101111010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [239, 0b111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [241, 0b10011110100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [251, 0b101010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [257, 0b101111010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [263, 0b111111100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [269, 0b110000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [271, 0b11111000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [277, 0b11101101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [281, 0b110010000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [283, 0b11100110100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [289, 0b11110011101011101100001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [293, 0b110110100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [307, 0b10101000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [311, 0b100101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [313, 0b11010001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [317, 0b101010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [331, 0b10101111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [337, 0b11100111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [347, 0b101011110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [349, 0b10100110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001],
    [353, 0b100010010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001]
  ]

-- TODO: Se puede acceder a cualquier polinomio irreducible de forma manual pero falta poder expresar una función del tipo
-- fq :: Integer -> Fq n ?



primeField :: Integer -> TH.TypeQ 
primeField n 
  | n<= 0 = error "galoisField: valor negativo"
  | isPrimeTrialDivision n = [t| Prime $(TH.litT (TH.numTyLit n)) |]

main :: IO ()
main = do 
  print "Ingrese un número primo: "
  pIrr <- readLn :: IO Natural 
  a <- readLn ::IO Integer 
  print a 
  return ()
