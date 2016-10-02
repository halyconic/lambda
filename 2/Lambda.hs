{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

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

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

newtype Fix f = Fix { unFix :: f (Fix f) }

-- catamorphism
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- anamorphism
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- hylomorphism
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo f g = cata f . ana g

data ExprF a b
  = Var a
  | Abst a b
  | App b b
  | Sub b a b 

type Expr a = Fix (ExprF a)

instance Functor (ExprF expr) where
  fmap f (Var a) = Var a
  fmap f (App a b) = App (f a) (f b)
  fmap f (Abst a b) = Abst a (f b)
  fmap f (Sub a b c) = Sub (f a) b (f c)

eval :: Eq a => Expr a -> Expr a
eval = cata phi where
  phi (App (Fix (Abst var body)) env) = Fix (Sub body var env)
  phi (Abst var (Fix (App body (Fix (Var var')))))
     | var == var' = body
  phi (Sub (Fix (Var var')) var env)
     | var == var' = env
     | otherwise = Fix (Var var')
  phi (Sub (Fix (Abst var' body)) var env)
     | var == var' = Fix (Abst var body)
     | otherwise = Fix (Abst var' (Fix (Sub body var env)))
  phi (Sub (Fix (App a b)) var env) = Fix (App (Fix (Sub a var env)) (Fix (Sub b var env)))
  phi expr = Fix expr

write' :: Expr String -> String
write' = cata phi where
  phi (Var x) = x
  phi (App a b) = "(" ++ a ++ b ++ ")"
  phi (Abst x b) = "\\" ++ x ++ "." ++ b
  phi (Sub a x c) = a ++ "[" ++ x ++ ":=" ++ c ++ "]"

write'' :: Expr String -> String
write'' = cata phi where
  phi (Var x) = x
  phi (App a b) = "(" ++ a ++ b ++ ")"
  phi (Abst x b) = "{" ++ x ++ "}" ++ b
  phi (Sub a x c) = a ++ "[" ++ x ++ ":=" ++ c ++ "]"

-- usage:
--
-- λ> write' $ eval (Fix $ App (Fix $ Abst "x" (Fix $ Var "x")) (Fix $ Abst "x" (Fix $ Var "x")))
-- "x[x:=\\x.x]"
-- λ> write' $ eval $ eval (Fix $ App (Fix $ Abst "x" (Fix $ Var "x")) (Fix $ Abst "x" (Fix $ Var "x")))
-- "\\x.x"

s, k, i :: Expr String
i = Fix $ Abst "x" (Fix $ Var "x")
k = Fix $ Abst "x" (Fix $ Abst "y" (Fix $ Var "x"))
s = Fix $ Abst "x" (Fix $ Abst "y" (Fix $ Abst "z" (Fix $ App (Fix $ App (Fix $ Var "x") (Fix $ Var "z")) (Fix $ App (Fix $ Var "y") (Fix $ Var "z")))))
