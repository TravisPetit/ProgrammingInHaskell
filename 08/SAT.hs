-- Exercise 8
import Data.List

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop
    | Or Prop Prop
    | Iff Prop Prop -- if and only if
    deriving Show

type Assoc a b = [(a, b)]
type Subst = Assoc Char Bool

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k',v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find' x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Iff p q) = eval s (Imply p q) && eval s (Imply q p)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Iff p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = nub (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Tests
a = isTaut (Const True)  -- Expect True
b = isTaut (Const False)  -- Expect False
c = isTaut (Var 'A')  -- Expect False
d = isTaut (Imply (Var 'A') (Var 'A'))  -- Expect True
e = isTaut (And (Var 'A') (Not (Var 'A')))  -- Expect False
f = isTaut (Or (Var 'A') (Not (Var 'A')))  -- Expect True
g = isTaut (Iff (Var 'A') (Var 'A'))  -- Expect True
h = isTaut (Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B'))  -- Expect True

