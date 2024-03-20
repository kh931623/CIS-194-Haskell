module Fibonacci
  ( fib,
    fibs1,
  )
where

fib :: Integer -> Integer
fib n
  | n <= 1 = n
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]
