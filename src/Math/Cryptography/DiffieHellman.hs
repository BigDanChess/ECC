{-#LANGUAGE NoImplicitPrelude, OverloadedStrings#-}
-- |
-- Module      : Diffie-Hellman
-- Description : Intercambio de clave Diffie-Hellman
-- Copyright   : (c) Daniel Alejandro Reinoso, 2024
-- License     : GPL-3
-- Maintainer  : danixreinoso@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Intercambio de clave de Diffie-Hellman sobre la curva elíptica SECT11ER1.
module Math.Cryptography.DiffieHellman where

import Math.EllipticCurve.Curve.Binary.SECT113R1 as CurveSECT113R1
import qualified Math.Algebra.Galois as Fr
import Protolude

-- | Genera un punto aleatorio privado en la curva SECT11ER1
generate :: IO CurveSECT113R1.Fr
generate = Fr.rnd

main :: IO ()
main = do
  alice_private <- generate
  bob_private <- generate
  let bob_public :: CurveSECT113R1.PA
      bob_public = gen `mul` bob_private
      alice_public :: CurveSECT113R1.PA
      alice_public = gen `mul` alice_private
  let shared_secret1 = bob_public `mul` alice_private
      shared_secret2 = alice_public `mul` bob_private
  putText "Entero privado de Alice:"
  print alice_private
  putText "Punto de Alice:"
  print alice_public
  putText "Entero privado de Bob:"
  print bob_private
  putText "Punto de Bob:"
  print bob_public
  putText "Secreto compartido de Diffie Hellman"
  print shared_secret1
  print (shared_secret1 == shared_secret2)
  pure ()