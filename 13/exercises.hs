import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

-- Exercise 1
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

notChar :: Char -> Parser Char
notChar x = sat (/= x)

comment :: Parser ()
comment = do
  char '-'
  char '-'
  many $ notChar '\n'
  -- In my opinion, the newline character is part of the comment:
  (char '\n' <|> empty)
  return ()

-- Exercises 2 & 3: see pdf

-- Exercise 5:
data Expr = Add Term Expr | ExprTerm Term

data Term = Mul Factor Term | TermFactor Factor

data Factor = Paren Expr | Number Int

instance Show Expr where
  show (Add x y) = show x ++ "+" ++ show y
  show (ExprTerm t) = show t

instance Show Term where
  show (Mul x y) = show x ++ "*" ++ show y
  show (TermFactor f) = show f

instance Show Factor where
  show (Paren e) = "(" ++ show e ++ ")"
  show (Number i) = show i

number :: Parser Factor
number = do
  n <- int
  return $ Number n

expr :: Parser Expr
expr =
  ( do
      t <- term
      symbol "+"
      e <- expr
      return (Add t e)
  )
    <|> fmap ExprTerm term

term :: Parser Term
term =
  ( do
      f <- factor
      symbol "*"
      t <- term
      return $ Mul f t
  )
    <|> fmap TermFactor factor

factor :: Parser Factor
factor =
  ( do
      symbol "("
      e <- expr
      symbol ")"
      return $ Paren e
  )
    <|> number

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

symbol :: String -> Parser String
symbol xs = token (string xs)

integer :: Parser Int
integer = token int

digit :: Parser Char
digit = sat isDigit

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- Exercise 6:

expr' :: Parser Int
expr' = do
  t <- term'
  do
    symbol "+"
    e <- expr'
    return (t + e)
    <|> do
      symbol "-"
      e <- expr'
      return (t - e)
    <|> return t

term' :: Parser Int
term' = do
  f <- factor'
  do
    symbol "*"
    t <- term'
    return (f * t)
    <|> do
      symbol "/"
      t <- term'
      return (f `div` t)
    <|> return f

factor' :: Parser Int
factor' =
  do
    symbol "("
    e <- expr'
    symbol ")"
    return e
    <|> integer

-- Exercise 7:

-- we change the grammar s.t. now
--expr   ::= term (+ expr | - expr | ε)
--term   ::= exponent (* term | / term | ε)
--exponent ::= factor (^ exponent | ε)
--factor ::= ( expr ) | int
--int    ::= ... | -1 | 0 | 1 | ...

expr'' :: Parser Int
expr'' =
  do
    t <- term''
    do
      symbol "+"
      e <- expr''
      return (t + e)
      <|> do
        symbol "-"
        e <- expr''
        return (t - e)
      <|> return t

term'' :: Parser Int
term'' =
  do
    e <- exponent''
    do
      symbol "*"
      t <- term''
      return (e * t)
      <|> do
        symbol "/"
        t <- term''
        return (e `div` t)
      <|> return e

exponent'' :: Parser Int
exponent'' =
  do
    f <- factor''
    ( do
        symbol "^"
        e <- exponent''
        return (f ^ e)
      )
      <|> return f

factor'' :: Parser Int
factor'' =
  do
    symbol "("
    e <- expr''
    symbol ")"
    return e
    <|> integer

-- Exercise 8:

-- 8a)
--expr ::= nat (-expr|e)

--8b)
expr8 :: Parser Int
expr8 =
  do
    n <- nat
    ( do
        symbol "-"
        m <- expr8
        return $ n - m
      )
      <|> return n

-- 8c) The problem with the parser is that the expression 1-2-3 gets parsed into 2 instead of -4.
-- The reason for that is that
-- parse "1-2-3" -> parse (1-2,-3)
--                = parse (-1, -3)
--               -> parse (-1-(-3),"")
--                = parse (2, "")
-- which is obv wrong

--8d)
expr8' :: Parser Int
expr8' = do
  n <- nat
  rest <-
    many
      ( do
          symbol "-"
          nat
      )
  return $ foldl (-) n rest
