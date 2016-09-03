module Eval where

import Data.Maybe
import Data.Tuple
import Data.List

-- reduction rules:
--
-- beta
-- (App (Abst var body) env) -> (Sub body var env)
--
-- eta
-- (Abst var (App body var')) -> body
--
-- alpha
-- (Sub (Var var) var env) -> env
-- (Sub (Var var') var env) -> (Var var')
-- (Sub (Abst var body) var env) -> (Abst var body)
-- (Sub (Abst var' body) var env) -> (Abst var' (Sub body var env))
-- (Sub (App a b) var env) -> (App (Sub a var env) (Sub b var env))

data Expr a
  = Var a
  | Abst a (Expr a)
  | App (Expr a) (Expr a)
  | Sub (Expr a) a (Expr a)
  deriving (Eq, Show)

app :: (Eq a) => Expr a -> Expr a
app (App (Abst var body) env) = (Sub body var env)
app a = a

abst :: (Eq a) => Expr a -> Expr a
abst (Abst var (App body (Var var')))
  | var == var' = body
abst a = a

sub :: (Eq a) => Expr a -> Expr a
sub (Sub (Var var') var env)
  | var == var' = env
  | otherwise = (Var var')
sub (Sub (Abst var' body) var env)
  | var == var' = (Abst var body)
  | otherwise = (Abst var' (Sub body var env))
sub (Sub (App a b) var env) = (App (Sub a var env) (Sub b var env))
sub a = a

eval :: (Eq a) => Expr a -> Expr a
eval expr@(Var _) = expr
eval expr@(Abst a b) = abst (Abst a (eval b))
eval expr@(App a b) = app (App (eval a) (eval b))
eval expr@(Sub a b c) = sub (Sub (eval a) b (eval c))

evalWhile :: (Eq a) => Expr a -> Expr a
evalWhile a
  | a == b = a
  | otherwise = evalWhile b
  where b = eval a

-- usage:
--
-- λ> evalWhile (App (App (App s k) k) k)
-- Abst "x" (Abst "y" (Var "x"))

s, k, i :: Expr String
i = Abst "x" (Var "x")
k = Abst "x" (Abst "y" (Var "x"))
s = Abst "x" (Abst "y" (Abst "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))
