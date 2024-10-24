-- Used to measure the error
epsilon :: Double
epsilon = 0.01 -- change this to whatever you want

-- ban the action x / y if y is small
-- Making this smaller means more divisions are allowed but can yield strange results due to numeric inaccuracies
tolerance :: Double
tolerance = 0.001

-- metric space
type Metric a = a -> a -> Double

-- I am going to use L1 to measure quality but ofc it would also make sense to use L2 or any other metric
l1 :: Metric Double
l1 x y = abs (x - y)

-- Only accept a solution if it is at most epsilon away from the target we want
solutionOK :: Double -> Double -> Bool
solutionOK candidate target = l1 candidate target <= epsilon

almost :: Double -> Double -> Bool
almost x y = l1 x y <= tolerance

notAlmost :: Double -> Double -> Bool
notAlmost x y = not (almost x y)

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
   show Exp = "^"

apply :: Op -> Double -> Double -> Double
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x / y
apply Exp x y = x ** y

data Expr = Val Double | App Op Expr Expr

instance Show Expr where
   show (Val n)     = show (round n)
   show (App o l r) = brak l ++ show o ++ brak r
                      where
                         brak (Val n) = show (round n)
                         brak e       = "(" ++ show e ++ ")"

eval :: Expr -> [Double]
eval (Val n)     = [n]
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

type Result = (Expr,Double)

implies :: Bool -> Bool -> Bool
implies x y = not x || y

valid :: Op -> Double -> Double -> Bool
valid Add x y = x <= y
valid Sub _ _ = True
valid Mul x y = notAlmost 1.0 x && notAlmost 1.0 y && x <= y
valid Div x y = notAlmost 1.0 y && notAlmost 0.0 y
valid Exp x y = notAlmost 1.0 x && (almost 0.0 x `implies` notAlmost 0.0 y)

results :: [Double] -> [Result]
results []  = []
results [n] = [(Val n,n)]
results ns  = [res | (ls,rs) <- split ns,
                       lx     <- results ls,
                       ry     <- results rs,
                       res    <- combine lx ry]
ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, (e,m) <- results (map fromIntegral ns'),  solutionOK m (fromIntegral n)]

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger
                where
                smaller = [y | y <- xs, y <= x]
                larger = [y | y <- xs, y > x]

countOps :: Expr -> Int
countOps (Val _) = 0
countOps (App _ x y) = 1 + countOps x + countOps y

instance Eq Expr where
  e1 == e2 = countOps e1 == countOps e2

instance Ord Expr where
    e1 < e2 = countOps e1 < countOps e2
    e1 <= e2 = countOps e1 <= countOps e2

orderedSolutions :: [Int] -> Int -> [Expr]
orderedSolutions ns n = take 10 (qSort (solutions ns n))

-- Example run:
-- orderedSolutions [1,2,3,4,5] 1 yields
-- [1,2-1,3-2,4-3,5-4,3-(2^1),(3^1)-2,(3-1)/2,3/(1+2),2/(3-1)]
