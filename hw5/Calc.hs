module Calc where

import ExprT
import Parser

--Ex1

eval :: ExprT -> Integer
eval ( Add x y ) = eval x + eval y
eval ( Mul x y ) = eval x * eval y
eval ( Lit x ) = x

--Ex2

evalStr :: String -> Maybe Integer
evalStr = go . parseExp Lit Add Mul
  where go ( Just x ) = Just $ eval x
        go Nothing = Nothing

-- Ex3

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Ex4

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (<=) 0
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y)= MinMax (max x y)
  mul (MinMax x) (MinMax y)= MinMax (min x y)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 ((x `mod` 7) + ( y `mod` 7 ))
  mul (Mod7 x) (Mod7 y) = Mod7 ((x `mod` 7) * ( y `mod` 7 ))

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + -5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
