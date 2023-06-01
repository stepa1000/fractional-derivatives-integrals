module Data.Fractional.Differintegral where

import Data.Fractional.Integrals
import Data.Utils
import Math.Gamma

differintegral ::
  (Fractional a, Enum a, Gamma a, RealFrac a, Ord a) =>
  -- | epsilon
  a ->
  -- | alfa 1
  a ->
  -- | alfa 2
  a ->
  -- | p
  a ->
  (a -> a) ->
  a ->
  a
differintegral e alf1 alf2 p f x
  | p > 0 = (derivationN (round $ p + alf2) e f x) * (integralRiemannLiouvilleLeft e alf2 alf1 f x)
  | p < 0 = integralRiemannLiouvilleLeft e (abs p) alf1 f x
