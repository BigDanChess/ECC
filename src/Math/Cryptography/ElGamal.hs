{-#LANGUAGE NoImplicitPrelude, OverloadedStrings#-}
-- |
-- Module      : ElGamal
-- Description : Cifrado ElGamal
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Cifrado de ElGamal sobre la curva SECT11ER1.
module Math.Cryptography.ElGamal where

import Math.EllipticCurve.Curve.Binary.SECT113R1 as CurveSECT113R1
import qualified Math.Algebra.Galois as Fr

import Protolude hiding (show)


import Data.Char (ord)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Prelude (String, read, show)


import Math.Algebra.GaloisFields.Binary

-- | Genera un punto aleatorio privado en la curva SECT11ER1
generate :: IO CurveSECT113R1.Fr
generate = Fr.rnd

encodeToF :: String -> F2n 
encodeToF = foldl' (\acc c -> acc * 256 + fromIntegral (ord c)) 0 

decode :: F2n -> String 
decode = decode' . toInteger

decode' :: Integer -> String
decode' 0 = ""
decode' n = decode' (n `div` 256) ++ [chr $ fromIntegral (n `mod` 256)]

mensaje :: String
mensaje = "Esto es una prueba"

splitM :: String -> (String, String)
splitM text = (s1,s2)
  where longitud = div (length text) 2
        (s1, s2) = splitAt longitud (text)


encodeToF2 :: String -> (F2n,F2n)
encodeToF2 text = (encodeToF s1, encodeToF s2)
  where (s1,s2) = splitM text 

decodeFromF2 :: (F2n, F2n) -> String
decodeFromF2 (f1,f2) = decode f1 ++ decode f2 

xCor,yCor :: CurveSECT113R1.PA -> F2n
xCor (A x y) = x 
yCor (A x y) = y 

descrifradoElGamal :: CurveSECT113R1.PA -> CurveSECT113R1.Fr -> F2n -> F2n -> String 
descrifradoElGamal b2 key c1 c2 = 
  case b2 `mul` key of 
    (A x y) -> decodeFromF2 (x'*c1,y'*c2)
      where 
        x' = recip x 
        y' = recip y

main :: IO ()
main = do
  -- | a 
  alice_private <- generate
  -- | k
  bob_randomInteger <- generate
      -- | A = aP
  let alice_public :: CurveSECT113R1.PA
      alice_public = gen `mul` alice_private
      -- | B1 = kP
      bob_public :: CurveSECT113R1.PA 
      bob_public = gen `mul` bob_randomInteger
      -- | B2 = kA
      bob_public2 :: CurveSECT113R1.PA
      bob_public2 = alice_public `mul` bob_randomInteger
      (m1,m2) = encodeToF2 mensaje 
      (c1,c2) = (x*m1,y*m2)
        where x = xCor bob_public2
              y = yCor bob_public2
      texto_cifrado = (bob_public,c1,c2)
  let 
  putText "Clave privada de Alice:"
  print alice_private
  putText "Clave pública de Alice:"
  print alice_public
  putText "Entero aleatorio de Bob:"
  print bob_randomInteger
  putText "Punto de Bob:"
  print bob_public
  putText "TextoCifrado:"
  print bob_public
  print c1 
  print c2 
  putText "TextoDescifrado:"
  print $ descrifradoElGamal bob_public alice_private c1 c2 
  pure ()




