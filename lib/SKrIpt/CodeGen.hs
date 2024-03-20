{-# LANGUAGE UnboxedTuples #-}

module SKrIpt.CodeGen where

import SKrIpt.Compiler
import SKrIpt.Runtime
import SKrIpt.PrimLib

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map.Lazy as Map

toAny :: Env -> Expr -> Any
toAny n (Cond :$ c :$ t :$ e) = acond3 (# toAny n c, toAny n t, toAny n e #)
toAny n (Cond :$ c :$ t) = unsafeCoerce acond2 (# toAny n c, toAny n t #)
toAny n (S :$ f :$ g :$ x) = s3 (# toAny n f, toAny n g, toAny n x #)
toAny n (S :$ f :$ g) = unsafeCoerce s2 (# toAny n f, toAny n g #)
toAny n (K :$ x :$ y) = k2 (# toAny n x, toAny n y #)
toAny n (B :$ f :$ g :$ x) = b3 (# toAny n f, toAny n g, toAny n x #)
toAny n (B :$ f :$ g) = unsafeCoerce b2 (# toAny n f, toAny n g #)
toAny n (C :$ f :$ g :$ x) = c3 (# toAny n f, toAny n g, toAny n x #)
toAny n (C :$ f :$ g) = unsafeCoerce c2 (# toAny n f, toAny n g #)
toAny n (C' :$ c :$ f :$ g :$ x) = c'4 (# toAny n c, toAny n f, toAny n g, toAny n x #)
toAny n (C' :$ c :$ f :$ g) = unsafeCoerce c'3 (# toAny n c, toAny n f, toAny n g #)
toAny n (C' :$ c :$ f ) = unsafeCoerce c'2 (# toAny n c, toAny n f #)
toAny n (S' :$ c :$ f :$ g :$ x) = s'4 (# toAny n c, toAny n f, toAny n g, toAny n x #)
toAny n (S' :$ c :$ f :$ g) = unsafeCoerce s'3 (# toAny n c, toAny n f, toAny n g #)
toAny n (S' :$ c :$ f ) = unsafeCoerce s'2 (# toAny n c, toAny n f #)
toAny n (BStar :$ c :$ f :$ g :$ x) = bStar4 (# toAny n c, toAny n f, toAny n g, toAny n x #)
toAny n (BStar :$ c :$ f :$ g) = unsafeCoerce bStar3 (# toAny n c, toAny n f, toAny n g #)
toAny n (BStar :$ c :$ f ) = unsafeCoerce bStar2 (# toAny n c, toAny n f #)
toAny n (f :$ x) = unsafeCoerce (toAny n f) (toAny n x)
toAny _ S = unsafeCoerce s
toAny _ S' = unsafeCoerce s'
toAny _ K = unsafeCoerce k
toAny _ I = unsafeCoerce i
toAny _ Y = unsafeCoerce y
toAny _ B = unsafeCoerce b
toAny _ BStar = unsafeCoerce bStar
toAny _ C = unsafeCoerce c
toAny _ C' = unsafeCoerce c'
toAny _ U = unsafeCoerce u
toAny _ Cond = unsafeCoerce acond
toAny _ (IntLit n) = unsafeCoerce n
toAny _ (BoolLit b) = unsafeCoerce b
toAny _ P = unsafeCoerce apair
toAny e (Var n) | Just v <- Map.lookup n e = v
toAny _ x = error $ show x

unsafeToVal :: Env -> Expr -> c
unsafeToVal e = unsafeCoerce . toAny e