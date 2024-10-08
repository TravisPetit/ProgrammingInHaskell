{-
Exercise 2:
    (2^3)*4
    (2*3)+(4*5)
    2+(3*(4^5))
-}

-- Exercise 3:
n = a `div` length xs
    where
    a = 10
    xs = [1,2,3,4,5]
-- errors:
-- variables must start with a lowercase letter so change N to n
-- the indentation was wrong
-- An infix div needs to be enclosed in back ticks, not single quotes

-- Exercise 4:
last' :: [a] -> a
last' = head . reverse

last'' :: [a] -> Maybe a
last'' [] = Nothing
last'' [x] = Just x
last'' (x:xs) = last'' xs

-- Exercise 5:
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

init'' :: [a] -> [a]
init'' [] = []
init'' [_] = []
init'' (x:xs) = x : (init'' xs)
