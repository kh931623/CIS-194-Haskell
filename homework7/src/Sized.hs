{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance (Sized b) => Sized (a, b) where
  size = size . snd

instance Semigroup Size where
  (Size i1) <> (Size i2) = Size (i1 + i2)

instance Monoid Size where
  mempty = Size 0
  mappend = (+)

getSizeFromSized :: (Sized a) => a -> Int
getSizeFromSized = getSize . size
