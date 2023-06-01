-- | https://www.youtube.com/watch?v=2dwQUUDt5Is
--
-- https://downloads.hindawi.com/journals/mpe/2014/238459.pdf
module Data.Fractional.Derivatives where

import Data.Utils
import Math.Gamma

derivativeLiouvilleLeft ::
  (Gamma a, Enum a) =>
  -- | differencial N
  Int ->
  -- | epsilon
  a ->
  -- | alfa
  a ->
  (a -> a) ->
  a ->
  a
derivativeLiouvilleLeft i e alf f x =
  (1 / gamma ((realToFrac i) - alf))
    * (derivationN i e f x)
    * ( integration
          e
          0
          x
          (\dze -> (x - dze) ** ((realToFrac i) - alf - 1) * (f dze))
      )

derivativeLiouvilleLeftN :: (Gamma a, Enum a) => Int -> Int -> a -> a -> (a -> a) -> a -> a
derivativeLiouvilleLeftN i j eps alf f x
  | i == 1 = derivativeLiouvilleLeft j eps alf f x
  | i > 1 = derivativeLiouvilleLeftN (i - 1) j eps alf (derivativeLiouvilleLeft j eps alf f) x
derivativeLiouvilleLeftN _ _ _ _ _ _ = error "wrong parameters"

derivativeRiemannLiouvilleLeft :: (Gamma a, Enum a) => Int -> a -> a -> a -> (a -> a) -> a -> a
derivativeRiemannLiouvilleLeft i e alf aPlus f x =
  (1 / gamma ((realToFrac i) - alf))
    * (derivationN i e f x)
    * ( integration
          e
          aPlus
          x
          (\dze -> (x - dze) ** ((realToFrac i) - alf - 1) * (f dze))
      )

derivativeRiemannLiouvilleLeftN :: (Gamma a, Enum a) => Int -> Int -> a -> a -> a -> (a -> a) -> a -> a
derivativeRiemannLiouvilleLeftN i j eps alf aPlus f x
  | i == 1 = derivativeRiemannLiouvilleLeft j eps alf aPlus f x
  | i > 1 = derivativeRiemannLiouvilleLeftN (i - 1) j eps alf aPlus (derivativeRiemannLiouvilleLeft j eps alf aPlus f) x
derivativeRiemannLiouvilleLeftN _ _ _ _ _ _ _ = error "wrong parameters"

derivativeRiemannLiouvilleRight :: (Gamma a, Enum a) => Int -> a -> a -> a -> (a -> a) -> a -> a
derivativeRiemannLiouvilleRight i e alf bm f x =
  ((-1) ^ i / gamma ((realToFrac i) - alf))
    * (derivationN i e f x)
    * ( integration
          e
          x
          bm
          (\dze -> (x - dze) ** ((realToFrac i) - alf - 1) * (f dze))
      )

derivativeRiemannLiouvilleRightN :: (Gamma a, Enum a) => Int -> Int -> a -> a -> a -> (a -> a) -> a -> a
derivativeRiemannLiouvilleRightN i j eps alf bm f x
  | i == 1 = derivativeRiemannLiouvilleRight j eps alf bm f x
  | i > 1 = derivativeRiemannLiouvilleRightN (i - 1) j eps alf bm (derivativeRiemannLiouvilleRight j eps alf bm f) x
derivativeRiemannLiouvilleRightN _ _ _ _ _ _ _ = error "wrong parameters"
