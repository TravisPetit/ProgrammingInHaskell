-- Exercise 1
--
-- Redexes in 1 + (2*3):
-- 2*3 -> innermost
--
-- Redexes in (1+2) * (2+3)
-- 1+2 -> innermost
-- 2+3 -> innermost
--
-- Redexes in (\x -> 1 + x) (2*3)
-- 2*3 -> innermost
-- (\x -> 1 + x) (2*3) -> outermost
--
-- Exercise 2
--
-- Using innermost evaluation:
--   fst (1+2,2+3)
-- = fst (3, 2+3)
-- = fst (3, 5)
-- = 3
-- with a total of 2 intermediary operations
--
-- Using outermost evaluation:
--   fst (1+2,2+3)
-- = 1+2
-- = 3
-- with a total of 1 intermediary operation
--
-- Exercise 3
--
--   mult 3 4
-- = \x -> (\y -> x * y) 3 4
-- = (\y -> 3 * y) 4
-- = 3 * 4
-- = 12

-- Exercise 4
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- Exercise 5
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

repeatTree :: a -> Tree a
repeatTree x = Node (repeatTree x) x (repeatTree x)

takeSubtree :: Int -> Tree a -> Tree a
takeSubtree _ Leaf = Leaf
takeSubtree 0 _ = Leaf
takeSubtree n (Node l x r) = Node (takeSubtree (n - 1) l) x (takeSubtree (n - 1) r)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeSubtree n . repeatTree

-- Exercise 6
guess :: Double
guess = 1.0

epsilon :: Double
epsilon = 0.00001

type Metric = Double -> Double -> Double

l1 :: Metric
l1 x y = abs $ x - y

next :: Double -> Double -> Double
next n a = (a + n / a) / 2

sqroot :: Double -> Double
sqroot x =
  let guesses = iterate (next x) guess
      dist = [l1 u v | (u, v) <- zip guesses (tail guesses)]
      dist' = takeWhile (>= epsilon) dist
      candidates = zip guesses dist'
   in fst $ last candidates
