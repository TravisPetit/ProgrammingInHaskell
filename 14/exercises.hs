-- Exercise 1
-- (Monoid a, Monoid b) is already an instance of monoid in the prelude so we need to change things up a bit

newtype Pair a b = Pair (a, b)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  --(<>) :: Pair a b -> Pair a b -> Pair a b
  Pair (x, y) <> Pair (z, w) = Pair (x <> z, y <> w)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  --mempty :: Pair (a, b)
  -- Note that both mempty need not be the same due to type inference
  mempty = Pair (mempty, mempty)

-- Exercise 2
newtype Function a b = Function (a -> b)

instance (Semigroup b) => Semigroup (Function a b) where
  --(<>) :: Funnction a b -> Function a b -> Function a b
  Function f <> Function g = Function h
    where
      h x = f x <> g x

-- Exercise 3

data Maybe' a = Just' a | Nothing'

instance Foldable Maybe' where
  --foldMap :: Monoid m => (a -> m) -> Maybe' a -> m
  foldMap f maybex = case maybex of
    Nothing' -> mempty
    Just' x -> f x

-- special case: foldMap id
fold :: Monoid m => Maybe' m -> m
fold maybem = case maybem of
  Nothing' -> mempty
  Just' x -> x

foldr :: (a -> b -> b) -> b -> Maybe' a -> b
foldr f y maybex = case maybex of
  Nothing' -> y
  Just' x -> f x y

foldl :: (b -> a -> b) -> b -> Maybe' a -> b
foldl f y maybex = case maybex of
  Nothing' -> y
  Just' x -> f y x

instance Functor Maybe' where
  --fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap f maybex = case maybex of
    Nothing' -> Nothing'
    Just' x -> Just' $ f x

--instance Traversable Maybe' where
instance Traversable Maybe' where
  --traverse :: Applicative f => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse g maybex = case maybex of
    Nothing' -> pure Nothing'
    Just' x -> pure (\u -> Just' u) <*> (g x)

-- Exercise 4

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  --fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf) = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

--traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r
instance Traversable Tree where
  --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l x r) =
    let builder :: Tree a -> a -> Tree a -> Tree a
        builder l' x' r' = Node l' x' r'
     in pure builder <*> traverse g l <*> g x <*> traverse g r

instance Foldable Tree where
  --foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = (f x) <> (foldMap f l) <> (foldMap f r)

-- Exercise 5

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p tx = foldMap (\x -> if p x then [x] else []) tx
