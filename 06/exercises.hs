-- Exercise 1:
-- Negative inputs make the function loop forever
-- E.g.
-- fib (-1) = fib (-3) + fib (-2)
--          = fib (-5) + fib (-4) + fib (-4) + fib (-3)
--          = ...
newfib :: Int -> Int
newfib n
    | n == 0 = 1
    | n == 1 = 1
    | n > 1  = newfib (n-2) + newfib (n-1)
    | otherwise = 0

-- Exercise 2:
sumdown :: Int -> Int
sumdown n
    | n == 0  = 0
    | n > 0  = n + sumdown (n -1)
    | otherwise = 0

-- Exercise 3:
power :: Int -> Int -> Int
power base exponent
    | exponent == 0 = 1
    | exponent > 0 = base * power base (exponent - 1)
    | otherwise = (-1) -- error

-- 2 ^ 3 = power 2 3
-- = 2 * power 2 2
-- = 2 * 2 * power 2 1
-- = 2 * 2 * 2 * power 2 0
-- = 2 * 2 * 2 * 1
-- = 6

-- Exercise 4:
euclid :: Int -> Int -> Int
euclid m n
    | m == n = m
    | m > n = euclid (m - n) n
    | m < n = euclid m (n - m)

-- Exercise 5:

-- length [1, 2, 3] = 1 + length [2, 3]
-- = 1 + 1 + length [3]
-- = 1 + 1 + 1 + length []
-- = 1 + 1 + 1 + 0
-- 3

-- drop 3 [1, 2, 3, 4, 5] = drop 2 [2, 3, 4, 5]
-- = drop 1 [3, 4, 5]
-- = drop 0 [4, 5]
-- = [4, 5]

-- init [1, 2, 3] = 1 : init [2, 3]
-- = 1 : 2 : init [3]
-- = 1 : 2 : []
-- = [1, 2]

-- Exercise 6:
and' :: [Bool] -> Bool
and' [] = True
and' (False:_) = False
and' (True:bs) = and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n - 1) x

select' :: [a] -> Int -> a
select' (x:_) 0 = x
select' (x:xs) n = select' xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = if x == y then True else elem' x ys


-- Exercise 7:
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x >= y = y : merge (x:xs) ys

-- Exercise 8:
halve :: [a] -> ([a], [a])
halve xs = ((take s xs), (drop s xs))
    where s = (length xs ) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right) where (left, right) = halve xs

-- Exercise 9:
sum' :: (Eq p, Num p) => p -> p
sum' 0 = 0
sum' n = n + sum' (n - 1)

take' :: (Eq p, Num p) => p -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x:(take' (n - 1) xs)

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
