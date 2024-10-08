{-
Exercise 1:
    double (double 2)
    double 2 + double 2
    double 2 + (2 + 2)
    double 2 + 4
    (2 + 2) + 4
    4 + 4
    8

Exercise 2:
    sum [x] = sum (x:[]) = x + sum [] = x + 0 = x
-}

-- Exercise 3:
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

{-
    product [2,3,4]
    product 2:(3:4:[])
    2 * product 3:(4:[])
    2 * 3 * product 4:[]
    2 * 3 * 4 * product []
    2 * 3 * 4 * 1
    24
-}

-- Exercise 4:
reverse_qsort :: Ord a => [a] -> [a]
reverse_qsort [] = []
reverse_qsort (x:xs) = reverse_qsort larger ++ [x] ++ reverse_qsort smaller
                        where
                            larger  = [a | a <- xs, a > x]
                            smaller = [a | a <- xs, a <= x]

{-
Exercise 5:
Replacing <= by < will remove duplicates while sorting, so e.g.
qsort [2,2,3,1,1] = [1,2,3]
-}
