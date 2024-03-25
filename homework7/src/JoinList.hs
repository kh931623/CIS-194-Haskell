module JoinList
  ( tag,
    (+++),
    indexJ,
  )
where

import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ l = l
l +++ Empty = l
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

indexJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  Maybe a
indexJ i _
  | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ v)
  | i == 0 = Just v
  | otherwise = Nothing
indexJ i (Append s l1 l2)
  | i >= len = Nothing
  | i < leftLen = indexJ i l1
  | otherwise = indexJ (i - leftLen) l2
  where
    len = getSizeFromSized s
    leftLen = (getSizeFromSized . tag) l1
