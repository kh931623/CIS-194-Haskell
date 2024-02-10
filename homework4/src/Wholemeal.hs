module Wholemeal
  ( fun1,
    fun1',
    fun2,
    fun2',
    compute,
    xor,
    map',
    foldTree,
    insert,
    Tree (Leaf, Node),
  )
where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

compute :: Integer -> Integer
compute 1 = 0
compute n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n = (sum . filter even . takeWhile (> 0) . iterate compute) n

singleXor :: Bool -> Bool -> Bool
singleXor x y = x /= y

xor :: [Bool] -> Bool
xor = foldl1 singleXor

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert n Leaf = Node 0 Leaf n Leaf
insert n (Node _ leftTree val Leaf) = Node 1 leftTree val (insert n Leaf)
insert n (Node level Leaf val rightNode) = Node level (insert n Leaf) val rightNode
insert n (Node level leftNode@(Node leftLevel _ _ _) val rightNode@(Node rightLevel _ _ _))
  | rightLevel == leftLevel = Node (newRightLevel + 1) leftNode val newRightNode
  | otherwise = Node level (insert n leftNode) val rightNode
  where
    newRightNode@(Node newRightLevel _ _ _) = insert n rightNode

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
