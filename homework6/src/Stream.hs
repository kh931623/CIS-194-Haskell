{-# LANGUAGE FlexibleInstances #-}

module Stream
  ( Stream (C),
    streamToList,
    streamRepeat,
    streamMap,
    streamFromSeed,
    nats,
    x,
  )
where

data Stream a = C a (Stream a)

streamToList :: Stream a -> [a]
streamToList (C v s) = v : streamToList s

instance (Show a) => Show (Stream a) where
  show = show . map show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat v = C v (streamRepeat v)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (C v s) = C (f v) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = C seed (streamFromSeed f nextSeed)
  where
    nextSeed = f seed

nats :: Stream Integer
nats = streamFromSeed (1 +) 0

-- ruler :: Stream Integer
-- ruler = streamMap compute nats

-- EX6 --
x :: Stream Integer
x = C 0 (C 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = C n (streamRepeat 0)

  negate = streamMap ((-1) *)

  (C v1 s1) + (C v2 s2) = C (v1 + v2) (s1 + s2)
