module Utils
  ( takeEvery,
  )
where

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = case dropped of
  [] -> []
  [x] -> [x]
  x : rl -> x : takeEvery n rl
  where
    dropped = drop (n - 1) xs
