module Data.Fractional.Integrals where

import Data.Utils
import Math.Gamma

integralRiemannLiouvilleLeft ::
  (Gamma a, Enum a) =>
  -- | epsilon
  a ->
  -- | alf or p
  a ->
  -- | aPlus
  a ->
  (a -> a) ->
  a ->
  a
integralRiemannLiouvilleLeft e alf aPlus f x =
  (1 / gamma alf)
    * (integration e aPlus x (\dze -> (x - dze) ** (alf - 1) * f dze))
