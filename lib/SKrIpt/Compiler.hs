module SKrIpt.Compiler where

type Name = String

data Expr
    = Expr :$ Expr
    | Where Expr [Def]
    | Let Def Expr
    | Var Name
    | IntLit !Integer
    | BoolLit !Bool
    | S | S' | K | I | B | BStar | C | C'   
    | Y 
    | P | U 
    | Cond
    deriving (Eq, Ord, Show)

infixl :$ 

data Def = Expr := Expr deriving (Eq, Ord, Show)

compile :: (Expr -> Expr) -> Expr -> Expr
compile opt = go where
    go (f :$ x) = go f :$ go x
    go (Where e defs) = optimize (removeWhere e defs)
    go (Let def e) = optimize (removeLet e def)
    go x = x

    optimize (f :$ x) = opt (optimize f :$ optimize x)
    optimize x = x

compileO0 = compile id
compileO1 = compile improve
compileO2 = compile improve2

abstract :: Expr -> Expr -> Expr
abstract x (Let def e) = abstract x (removeLet e def)
-- \x -> let f = bod in e === \x -> (\f -> e) (\f -> bod)
abstract x (Where e defs) = abstract x (removeWhere e defs)
-- \x -> e where f = bod1; g = bod2 === 
-- \x -> (\(f,g) -> e) (fix (\(f,g) -> (bod1,bod2)))
abstract x (f :$ y) = S :$ abstract x f :$ abstract x y
-- \x -> f y === S (\x -> f) (\x -> y)
abstract (P :$ x :$ y) (Var z) = U :$ abstract x (abstract y (Var z))
-- \(x, y) -> z === uncurry (\x -> \y -> z)
abstract var exp | var == exp = I
                 | otherwise = K :$ exp 
-- \x -> x === id
-- \x -> e === const e

removeWhere :: Expr -> [Def] -> Expr
removeWhere e defs = abstract f e :$ (Y :$ abstract f body) where
    f := body = collect (map absArgs defs)  
-- e where f = body === (\f -> e) (fix (\f -> body))
-- e where f = bod1; g = bod2 === e where (f, g) 
--                  === (\(f,g) -> e) (fix (\(f,g) -> (bod1,bod2)))

removeLet :: Expr -> Def -> Expr
removeLet e def = abstract f e :$ abstract f body where
    f := body = absArgs def

absArgs :: Def -> Def
absArgs d@((P :$ _ :$ _) := _) = d
absArgs ((f :$ x) := body) = absArgs (f := abstract x body)
-- f x = body === f = \x -> body
-- f x y = body === f = \x y -> body
absArgs x = x

collect :: [Def] -> Def
collect [def] = def
collect (def:defs) = f := body where
    f = P :$ f1 :$ f2
    body = P :$ body1 :$ body2
    f1 := body1 = def
    f2 := body2 = collect defs

improve :: Expr -> Expr
improve (S :$ (K :$ f) :$ (K :$ g)) = K :$ improve (f :$ g)
improve (S :$ (K :$ f) :$ I) = f
improve (S :$ (K :$ f) :$ g) = B :$ f :$ g
improve (S :$ f :$ (K :$ g)) = C :$ f :$ g
improve x = x

improve2 :: Expr -> Expr
improve2 (S :$ (K :$ f) :$ (K :$ g)) = K :$ improve2 (f :$ g)
improve2 (S :$ (K :$ f) :$ I) = f
improve2 (S :$ (K :$ f) :$ (B :$ q :$ r)) = BStar :$ f :$ q :$ r
improve2 (S :$ (K :$ f) :$ g) = B :$ f :$ g
improve2 (S :$ (B :$ p :$ q) :$ (K :$ g)) = C' :$ p :$ q :$ g
improve2 (S :$ f :$ (K :$ g)) = C :$ f :$ g
improve2 (S :$ (B :$ p :$ q) :$ r) = S' :$ p :$ q :$ r
improve2 x = x