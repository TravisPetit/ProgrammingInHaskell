-- Exercise 9
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
data Op = EVAL_ADD Expr | EVAL_MULT Expr | ADD Int | MULT Int

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL_ADD y : c)
eval (Mult x y) c = eval x (EVAL_MULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL_ADD e : c) n = eval e (ADD n : c)
exec (EVAL_MULT e : c) n = eval e (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value :: Expr -> Int
value e = eval e []

-- Test
-- (2 + 3) * 4 = 20
expr :: Expr
expr = Mult (Add (Val 2) (Val 3)) (Val 4)

result :: Int
result = value expr

--5 + (6 * 2)
expr2 :: Expr
expr2 = Add (Val 5) (Mult (Val 6) (Val 2))

result2 :: Int
result2 = value expr2

