{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  MinMax(..),
  Mod7(..),
  compile
) where

import Control.Applicative
import Homework.Week05.ExprT
import Homework.Week05.Parser
import qualified Homework.Week05.StackVM as VM

-- #1
eval :: ExprT -> Integer
eval (Lit v) = v
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- #2
evalStr :: String -> Maybe Integer
evalStr = parseExp id (+) (*)


-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- #4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = if a <= 0 then False else True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
  deriving (Show, Eq)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer
  deriving (Show, Eq)

instance Expr Mod7 where
  lit = Mod7 . mod7
  add (Mod7 a) (Mod7 b) = Mod7 (mod7 $ a + b)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod7 $ a * b)

mod7 :: Integer -> Integer
mod7 = flip mod 7

-- #5
instance Expr VM.Program where
  lit x = [VM.PushI x]
  add a b = VM.Add : a ++ b
  mul a b = VM.Mul : a ++ b

compile :: String -> Maybe VM.Program
compile = evalParser compiler

compiler :: Parser [VM.StackExp]
compiler = pushI <|> pushB

pushI :: Parser [VM.StackExp]
pushI = (\n -> [VM.PushI n]) <$> num

pushB :: Parser [VM.StackExp]
pushB =
  (pure [VM.PushB False] <* string "False") <|>
  (pure [VM.PushB True] <* string "True")

string :: String -> Parser String
string [] = pure []
string (x:xs) = liftA2 (:) (char x) (string xs)
