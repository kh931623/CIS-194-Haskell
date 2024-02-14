module Calc
  ( eval,
    evalStr,
    testInteger,
    testBool,
    testMinMax,
    testMod7,
  )
where

import ExprT
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just exp1) = Just $ eval exp1

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp Lit Add Mul

-- exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax n1) (MinMax n2) = MinMax (max n1 n2)
  mul (MinMax n1) (MinMax n2) = MinMax (min n1 n2)

instance Expr Mod7 where
  lit n = Mod7 (mod n 7)
  add (Mod7 n1) (Mod7 n2) = Mod7 (mod (n1 + n2) 7)
  mul (Mod7 n1) (Mod7 n2) = Mod7 (mod (n1 * n2) 7)

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMinMax = testExp :: Maybe MinMax

testMod7 = testExp :: Maybe Mod7