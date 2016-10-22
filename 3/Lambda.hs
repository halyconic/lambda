{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

import Data.Maybe
import Data.Tuple
import Data.List
import Data.Word

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
  | Abst b
  | App b b
  | Sub a b b
    
type Expr a = Fix (ExprF a)

instance Functor (ExprF expr) where
  fmap f (Var a) = Var a
  fmap f (Abst a) = Abst (f a)
  fmap f (App a b) = App (f a) (f b)
  fmap f (Sub a b c) = Sub a (f b) (f c)

eval :: Expr Word -> Expr Word
eval = cata phi where
  phi (App (Fix (Abst body)) env) = dec (Fix (Sub minBound (inc body) env))
  -- phi (Abst (Fix (App body (Fix (Var var')))))
  --   | var == var' = body
  phi (Sub var expr (Fix (Var var')))
     | var == var' = expr
     | otherwise = Fix $ Var var'
  phi (Sub var expr (Fix (App a b)))
     = Fix (App (Fix (Sub var a expr)) (Fix (Sub var b expr)))
  phi (Sub var expr (Fix (Abst expr')))
     = Fix $ Abst (Fix $ Sub (succ var) (inc $ expr) expr')
  phi expr = Fix expr

inc :: (Show a, Enum a) => Expr a -> Expr a
inc = cata phi where
  phi (Var a) = Fix $ Var (succ a)
  phi (Abst a) = Fix $ Abst a
  phi (App a b) = Fix $ App a b
  phi expr = Fix $ expr

dec :: (Show a, Enum a) => Expr a -> Expr a
dec = cata phi where
  phi (Var a) = Fix $ Var (pred a)
  phi (Abst a) = Fix $ Abst a
  phi (App a b) = Fix $ App a b
  phi expr = Fix $ expr

write' :: Show a => Expr a -> String
write' = cata phi where
  phi (Var x) = show $ x
  phi (App a b) = "(" ++ a ++ " " ++ b ++ ")"
  phi (Abst a) = "\\" ++ a
  phi (Sub x a c) = a ++ "[" ++ (show x) ++ ":=" ++ c ++ "]"

write'' :: Show a => Expr a -> String
write'' = cata phi where
  phi (Var x) = show $ x
  phi (App a b) = "(" ++ a ++ " " ++ b ++ ")"
  phi (Abst a) = "|" ++ a
  phi (Sub x a c) = a ++ "[" ++ (show x) ++ ":=" ++ c ++ "]"

-- usage:
--
-- λ> write' $ eval (Fix $ App (Fix $ Abst "x" (Fix $ Var "x")) (Fix $ Abst "x" (Fix $ Var "x")))
-- "x[x:=\\x.x]"
-- λ> write' $ eval $ eval (Fix $ App (Fix $ Abst "x" (Fix $ Var "x")) (Fix $ Abst "x" (Fix $ Var "x")))
-- "\\x.x"

s, k, i :: Expr Word
i = Fix $ Abst $ Fix $ Var 0
k = Fix $ Abst $ Fix $ Abst $ Fix $ Var 1
s = Fix $ Abst $ Fix $ Abst $ Fix $ Abst $ Fix $ (App (Fix $ App (Fix $ Var 2) (Fix $ Var 0)) (Fix $ App (Fix $ Var 1) (Fix $ Var 0)))
