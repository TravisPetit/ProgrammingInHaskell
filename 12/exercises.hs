--Exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  --fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

--Exercise 2
-- I have to create a new datatype beacuse -> is already a functor
newtype Function a b = F (a -> b)

unwrap :: Function a b -> a -> b
unwrap (F f) = f

instance Functor (Function x) where
  --fmap :: (a -> b) -> F x a -> F x b
  --      = (a -> b) -> (x -> a) -> (x -> b)
  fmap f g = F $ f . unwrap g

--Exercise 3
instance Applicative (Function x) where
  --pure :: a -> Function x a
  pure u = F $ \_ -> u

  -- (<*>) :: Function x (a -> b) -> Function x a -> Function x b
  --        = (x -> a -> b) -> (x -> a) -> x -> b
  -- f :: Function x (a -> b)
  -- u :: Function x a
  -- v :: x
  f <*> u = F $ \v -> (unwrap f v) (unwrap u v)

newtype ZipList a = Z [a] deriving (Show)

--Exercise 4
instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z $ map g xs

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z $ map (\_ -> x) [1 ..]

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z $ [g x | (g, x) <- zip gs xs]

--Exercise 5
--The applicative laws are
-- (1) pure id <*> x = x
-- (2) pure (g x) = pure g <*> pure x
-- (3) x <*> pure y = pure (\g -> g y) <*> x
-- (4) x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-- Recall that
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- In (1) we get
-- (pure id) <*> x implies that id :: a -> a  and that x :: f a

-- In (2) we get
-- pure g <*> pure x implies that g :: a -> b and x :: a

-- In (3) we get
-- x <*> pure y implies that x :: f (a -> b) and y :: a
--
-- In (4) we get
-- x :: f (a -> b) and y <*> z :: f a
-- from this we get y :: f (x -> a) and z :: f x

--Exercise 6
instance Monad (Function x) where
  -- (>>=) :: Function x a -> (a -> Function x b) -> Function x b
  ma >>= mf = F $ \u -> unwrap (mf $ unwrap ma u) u

--Exercise 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
  --fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var $ f x
  fmap f (Val i) = Val i
  fmap f (Add e e') = Add (fmap f e) (fmap f e')

instance Applicative Expr where
  --pure :: a -> Expr a
  pure x = Var x

  --(<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (Var f) <*> (Var x) = Var (f x)
  (Var f) <*> (Add e1 e2) = Add (fmap f e1) (fmap f e2)
  (Var f) <*> (Val i) = Val i
  (Val i) <*> _ = Val i
  (Add fe1 fe2) <*> e = Add (fe1 <*> e) (fe2 <*> e)

-- Try out the example from
-- https://stackoverflow.com/questions/79147327/how-to-implement-for-custom-type-in-haskell
(x, y) = (Var "x", Var "y")

(f, g) = (Var $ \x -> "f" ++ x, Var $ \x -> "g" ++ x)

(l, r) = (Add f (Add (Val 4) g), Add (Add (Val 1) x) y)

result = l <*> r

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= fe = fe x
  Val i >>= _ = Val i
  Add e1 e2 >>= fe = Add (e1 >>= fe) (e2 >>= fe)

-- Here is a nice example that chatgpt came up with:
-- Let's say you have an the expression 'r' from above
-- You want to substitute the value of x for 5
-- Monads make that super easy. Let
substitute :: String -> Expr String
substitute "x" = Val 1
substitute v = Var v

-- Then it is as easy as
r' = r >>= substitute

-- Tada! it's like magic

--Exercise 8
type State = Int -- can be whatever

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) s = st s

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = st >>= (return . g)

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = stf >>= (\f -> stx >>= (return . f))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f =
    S $ \s ->
      let (x, s') = app st s
       in app (f x) s'
