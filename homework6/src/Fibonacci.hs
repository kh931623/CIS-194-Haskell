module Fibonacci
  ( fib,
    fibs1,
    fibInf,
    fibs2,
    fibInf2,
  )
where

fib :: Integer -> Integer
fib n
  | n <= 1 = n
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibInf :: [Integer] -> [Integer]
fibInf l@(x : y : _) = newElement : fibInf (newElement : l)
  where
    newElement = x + y
fibInf _ = []

fibInf2 :: (Integer, Integer) -> [Integer]
fibInf2 (x, y) = newElement : fibInf2 (y, newElement)
  where
    newElement = x + y

fibs2 :: [Integer]
fibs2 = 0 : 1 : fibInf2 (0, 1)
