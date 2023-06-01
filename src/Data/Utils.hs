module Data.Utils where

derivation :: Fractional a => a -> (a -> a) -> a -> a
derivation e f x = (f (x + e) - (f x)) / e

derivationN :: Fractional a => Int -> a -> (a -> a) -> a -> a
derivationN i e f x
  | i == 1 = derivation e f x
  | i > 1 = derivationN (i - 1) e (derivation e f) x
  | i < 1 = error "parameters is wrong"
derivationN _ _ _ _ = error "error"

integration :: (Fractional a, Enum a) => a -> a -> a -> (a -> a) -> a
integration e l r f = sum $ g [l, l + e .. r]
  where
    g (x1 : x2 : xs) = (((f x1 + f x2) / 2) * (x2 - x1)) : (g (x2 : xs))
    g _ = []

-- integrationN :: (Fractional a, Enum a) => Int ->
