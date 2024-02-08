module Golf
  ( skips,
  )
where

import Utils (takeEvery)

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (flip takeEvery xs) params
  where
    len = length xs
    params = [1 .. len]
