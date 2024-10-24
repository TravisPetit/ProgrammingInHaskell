data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
   show Exp = "^"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
   show (Val n)     = show n
   show (App o l r) = brak l ++ show o ++ brak r
                      where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

choices :: [a] -> [[a]]
choices = concat . map perms . subs

type Result = (Expr,Int)

implies :: Bool -> Bool -> Bool
implies x y = not x || y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0
-- It does not make sense to have 1 ^ ... because 1^ anything = 1. Also, 0^0 is not allowed.
valid Exp x y = x /= 1 && ((x == 0) `implies` (y /= 0))

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                       lx     <- results ls,
                       ry     <- results rs,
                       res    <- combine lx ry]
ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

