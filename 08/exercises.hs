-- Exercise 1
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

-- Exercise 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r)
    | result == EQ = True
    | result == LT = occurs x l
    | otherwise = occurs x r
    where result = compare x y
-- This is better because x and y are only compared once. In the other version they can be compared once or twice

-- Exercise 3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

countLeaves :: Tree' a -> Int
countLeaves (Leaf' _) = 1
countLeaves (Node' x y) = countLeaves x + countLeaves y

absDist :: Int -> Int -> Int
absDist x y = abs (x - y)

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' x y) = absDist (countLeaves x) (countLeaves y) <= 1

-- Exercise 4
halve :: [a] -> ([a], [a])
halve xs = ((take s xs), (drop s xs))
    where s = (length xs ) `div` 2

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance u) (balance v) where (u,v) = halve xs

-- Exercise 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Exercise 6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- Exercise 7
-- I have to redefine maybe so I don't get an error:
data Maybe' a = Nothing' | Just' a deriving Show

instance Eq a => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Just' x == Just' y = (x == y)
    _ == _ = False
-- IMO it would also make sense to set
-- Nothing' == Nothing' = False
-
-- Exercises 7 and 8 in SAT.hs and machine.hs
