module Golf
  ( skips,
    localMaxima,
  )
where

import Utils (takeEvery)

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (flip takeEvery xs) params
  where
    len = length xs
    params = [1 .. len]

localMaxima :: [Integer] -> [Integer]
localMaxima (l : x : r : xs)
  | x > l && x > r = x : localMaxima rl
  | otherwise = localMaxima rl
  where
    rl = x : r : xs
localMaxima _ = []
