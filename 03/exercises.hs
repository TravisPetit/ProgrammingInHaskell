-- Exercise 1:
_ = ['a', 'b', 'c'] :: [Char]
_ = ('a', 'b', 'c') :: (Char, Char, Char)
_ = [(False,'O'),(True,'1')] :: [(Bool, Char)]
_ = ([False,True],['0','1']) :: ([Bool], [Char])
_ = [tail, init, reverse] :: [[a] -> [a]]

-- Exercise 2:
bools :: [Bool]
bools = [True, True, False]

nums :: [[Int]]
nums = [[], [1,2,3]]

add :: Int -> Int -> Int -> Int
add x y z = x

copy :: a -> (a,a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- Exercise 3:
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{- Exercises 5
It is not just infeasible, but rather impossible. Here's a short proof.
Let H be the type of all Haskell programs.
Let exec :: H -> Bool be a function that runs a program and returns True if it terminates and False otherwise.
Let f :: H -> Bool, f _ = True.
Then we could use exec p == f p to solve the Halting Problem, which we know is impossible.
Therefore a faithful implementation of Eq cannot exist.
-}
