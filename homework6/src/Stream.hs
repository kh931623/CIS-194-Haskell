module Stream
  ( Stream (C),
    streamToList,
    streamRepeat,
    streamMap,
    streamFromSeed,
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
