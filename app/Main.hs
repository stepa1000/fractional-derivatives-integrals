{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Fractional.Differintegral
import Graphics.Curves

-- import FMP
-- import FMP.Canvas

main :: IO ()
main =
  renderImage
    "./img.png"
    500
    500
    black
    ( (lineStrip $ (\(x, y) -> Vec x y) $ fmap (\a -> (differintegral 0.001 0 1 0.22 sin a, a)) $ fmap ((+) (negate 50)) [0, 1 .. 100])
        <> (lineStrip $ (\(x, y) -> Vec x y) $ fmap (\a -> (sin a, a)) $ fmap ((+) (negate 50)) [0, 1 .. 100])
    )

{-
main :: IO ()
main =
  funcmp
    "function"
    2
    [ cdraws @(Double, Double) $ fmap (\a -> (differintegral 0.001 0 1 0.22 sin a, a)) $ fmap ((+) (negate 50)) [0, 1 .. 100],
      cdraws @(Double, Double) $ fmap (\a -> (sin a, a)) $ fmap ((+) (negate 50)) [0, 1 .. 100]
    ]
-}
