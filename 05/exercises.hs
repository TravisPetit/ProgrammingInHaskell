-- Exercise 1
sum_of_squares_100 :: Int
sum_of_squares_100 = sum [x*x | x <- [1 .. 100]]

-- Exercise 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- Exercise 3
square :: Int -> [(Int, Int)]
square n = [xy | xy <- grid n n, fst xy /= snd xy]

-- Exercise 4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1 .. n]]

-- Exercise 5
isPythagorean :: Int -> Int -> Int -> Bool
isPythagorean x y z = x * x + y * y == z * z

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], isPythagorean x y z]

-- Exercise 6
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

isPerfect :: Int -> Bool
isPerfect n = n == sum (init (factors n))

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], isPerfect x]

-- Exercise 7
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = concat [[(x,y) | y <- ys] | x <- xs]

-- Exercise 8
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- Exercise 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

