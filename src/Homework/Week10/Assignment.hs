module Homework.Week10.Assignment (
  zeroOrMore,
  oneOrMore,
  spaces,
  ident,
  parseSExpr,
  Ident(..),
  Atom(..),
  SExpr(..)
) where

import Control.Applicative

import Homework.Week10.AParser

-- #1
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = undefined

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = undefined

-- #2
spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined

-- #3
type Ident = String

data Atom = N Integer 
          | I Ident
  deriving Show

data SExpr = A Atom 
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = undefined