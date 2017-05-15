module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval ( Add x y ) = eval x + eval y
eval ( Mul x y ) = eval x * eval y
eval ( Lit x ) = x

evalStr :: String -> Maybe Integer
evalStr = go . parseExp Lit Add Mul
  where go ( Just x ) = Just $ eval x
        go Nothing = Nothing

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul
