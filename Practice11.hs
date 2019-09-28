module Practice11 where

data Op = Add | Sub | Mul | Div deriving Show
data Expr = Val Int | App Op Expr Expr deriving Show

type Result = (Expr, Int)

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Div x y = (y/=1) && (x `mod` y == 0)
valid Mul x y = (x/=1) && (y/=1) && (x<=y)

apply :: Op -> Int -> Int -> Int
apply Add v1 v2 = v1 + v2
apply Sub v1 v2 = v1 - v2
apply Mul v1 v2 = v1 * v2
apply Div v1 v2 = v1 `div` v2

values :: Expr -> [Int]
values = undefined

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y] 

subs :: [a] -> [[a]]
subs [x] = [[x]]
subs (x:xs) = xss ++ [x] : map(x:) xss where xss = subs xs
subs [] = error "subs"

insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insert x) (perms xs)

choices = concatMap perms . subs

splits :: [a] -> [([a], [a])]
splits xs = [splitAt i xs | i <- [1..length xs - 1]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- splits ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

solutions :: Int -> [Int] -> [Expr]
solutions n ns = [e | ns1 <- choices ns, e <- exprs ns1, eval e == [n]]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [e | (ls, rs) <- splits ns, l <- results ls, r <- results rs, e <- combine1 l r]

solutions1 :: Int -> [Int] -> [Expr]
solutions1 n ns = [e | ns1 <- choices ns, (e, m) <- results ns1, m == n]

combine1 :: Result -> Result -> [Result]
combine1 (l, x) (r, y) = [(App o l r, apply o x y) | o <- [Add,Sub,Mul,Div], valid o x y]