-- Exercise 1
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

choices :: [a] -> [[a]]
choices [] = [[]]
choices (x:xs) = c ++ [w | ys <- c, w <- interleave x ys] where c = choices xs

-- Exercise 2
dropFirst :: Eq a => a -> [a] -> [a]
dropFirst _ [] = []
dropFirst x (y:ys) = if x == y then ys else y:(dropFirst x ys)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] xs = True
isChoice (x:xs) ys = (elem x ys) && isChoice xs (dropFirst x ys)

-- Exercise 3
-- It would loop forever, leading to non-termination. For example let f = results []. Then
-- f = reults [] = [res | (lx, ly) <- [([], [])], lx <- results ls, ry <- results rs, res <- combine lx ry]
--   = [res | lx <- results [], ry <- results [], res <- combine lx ry]
-- So f = [res | lx <- f, ry <- f, res <- combine lx ry]
-- Note how f is defined on terms of itself without a base case.

-- Exercise 4
ops :: [Op]
ops = [Add,Sub,Mul,Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

data Op = Add | Sub | Mul | Div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub _ _ = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]


exprs ::  [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]

-- modified to include a valid function f
eval :: (Op -> Int -> Int -> Bool) -> Expr -> [Int]
eval f (Val n)     = [n | n > 0]
eval f (App o l r) = [apply o x y | x <- eval f l,
                                    y <- eval f r,
                                    f o x y]

test :: [Int]
test = [1, 3, 7, 10, 25, 50]
testOK = length (exprs test) == 33665406 -- I get 43008 (??)

evals = concat (map (eval valid) (exprs test))
evalsOK = length evals == 4672540 -- I get 2238 (??)

-- Exercuse 5
evals2 = concat (map (eval valid') (exprs test))
evals2OK = length evals2 == 10839369 -- I get 11115 (??)

