-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Exercise 2
third_a :: [a] -> a
third_a = head . tail . tail

third_b :: [a] -> a
third_b xs = xs !! 2

third_c :: [a] -> a
third_c (_ : _ : x : _) = x

-- Exercise 3
safetail_a :: [a] -> [a]
safetail_a xs = (if null xs then [] else tail xs)

safetail_b :: [a] -> [a]
safetail_b xs | null xs   = []
              | otherwise = tail xs

safetail_c :: [a] -> [a]
safetail_c [] = []
safetail_c xs = tail xs

-- Exercise 4
lor_a :: Bool -> Bool -> Bool
lor_a True True   = True
lor_a True False  = True
lor_a False True  = True
lor_a False False = False

lor_b :: Bool -> Bool -> Bool
lor_b False False = False
lor_b _ _ = True

lor_c :: Bool -> Bool -> Bool
lor_c False b = b
lor_c True _ = True

lor_d :: Bool -> Bool -> Bool
lor_d b c | b == c    = b
          | otherwise = True

-- Exercise 5
land :: Bool -> Bool -> Bool
land b c = if b then if c then True else False else False

-- Exercise 6
land' :: Bool -> Bool -> Bool
land' b c = if b then c else False

-- Exercise 7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

-- Exercise 8
luhnDouble :: Int -> Int
luhnDouble x = if result > 9 then result - 9 else result
                where
                result = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = (luhnDouble z + luhnDouble x + w + y) `mod` 10 == 0
