-- Exercise 1
comprehension :: (a -> b) -> (b -> Bool) -> [a] -> [b]
comprehension f p = (filter p) . (map f)

-- Exercise 2
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x y -> p x && y) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x y -> p x || y) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x:xs else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = foldr (\x xs -> if p x then xs else x:xs) []

-- Exercise 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x:xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- Exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> y + 10 * x) 0

-- Exercise 5
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- Exercise 6
unfold p h t x
    | p x = []
    | otherwise = h x: unfold p h t (t x)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\x -> False) f f

-- Exercises 7 and 8 in transmitter.hs

-- Exercise 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x:y:xs) = (f x):(g y) : altMap f g xs

-- Exercise 10
luhnDouble :: Int -> Int
luhnDouble x = if result > 9 then result - 9 else result
                where
                result = 2*x

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . (altMap id luhnDouble) . reverse
