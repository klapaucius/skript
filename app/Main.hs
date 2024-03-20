module Main where

import SKrIpt.Compiler 
import SKrIpt.CodeGen 
import SKrIpt.PrimLib

instance Num Expr where
    a + b = Var "prelude#PlusInteger" :$ a :$ b
    a * b = Var "prelude#TimesInteger" :$ a :$ b
    a - b = Var "prelude#MinusInteger" :$ a :$ b
    fromInteger i = IntLit i
  
(===) :: Expr -> Expr -> Expr
a === b = Var "prelude#EqInteger" :$ a :$ b

fibD :: Def 
fibD = (Var "fib" :$ Var "n") := (Cond :$
    (Var "n" === 0) :$ 1 :$ (Cond :$
        (Var "n" === 1) :$ 1 :$ 
        ((Var "fib" :$ (Var "n" - 1)) + (Var "fib" :$ (Var "n" - 2)) + 1)))

fibE :: Expr -> Expr
fibE n = Where (Var "fib" :$ n) [fibD]

nfib :: Integer -> Integer
nfib n = if n < 2 then 1 else nfib(n-1) + nfib(n-2) + 1

main :: IO ()
main = do
    print ((unsafeToVal defEnv $ compileO2 (fibE 35)) :: Integer)
    -- print $ nfib 35
