{-# OPTIONS_GHC -Wall #-}
module Kundik06 where

import Data.Maybe
import qualified Data.Map as M

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
--   Всі оператори, функції і процедури застосовуються  
--      до вірної кількості аргументів, кожний з яких має відповідний тип.
--   Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
--     до 0 (false) або 1 (true).
--   В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
--   Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення 
--     (закінчує своє обчислення оператором return e) 
--   Оператор return завжди останній оператор для виконання в блоку процедури 
--     (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value | 
           Var Id | 
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp] 
         deriving (Eq, Show)

data VarDef = Arr Id | Int Id   deriving (Eq, Show)
type FunDef = (Id, ([VarDef], Exp))

type Binding = M.Map Id Value
type StateP = ([Binding],Binding)
-- st = ([locn,.. loc1], glob)  стек локальних записів активацій + глобальний запис активації

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp 
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Задача 1 -----------------------------------------
getValue ::  Id -> StateP -> Value
-- Передумова: Значення змінної Id є в стані StateP
getValue idv ([], gl)    = lookUp idv (M.toList gl)
getValue idv ((l:_), gl) = lookUp idv (M.toList $ M.union l gl)

-- Задача 2 -----------------------------------------
getLocals :: StateP -> Binding 
getLocals = head . fst

getGlobals :: StateP -> Binding
getGlobals = snd 

-- Задача 3 -----------------------------------------
assignArray :: Value -> Value -> Value -> Value
-- Аргументи - масив, індекс і (нове) значення відповідно
-- Передумова: Три аргумента (Value)  мають значення відповідного типу  
--     (масив (A),  ціле (I) і ціле (I)) відповідно.
assignArray (A a) (I i) (I e) = A (insert i e a)
assignArray _ _ _             = error "Невірно надані аргументи"

insert :: (Ord k, Eq a) => k -> a -> [(k, a)] -> [(k, a)]
insert k a xs = case res of
                  (xs', True)  -> xs'
                  (xs', False) -> (k,a):xs'
  where res = foldr (\e@(k',_) (acc, found) -> if found
                                                then (e:acc, found)
                                                else if k' == k 
                                                      then ((k, a):acc, True)
                                                      else (e:acc, False)) ([], False) xs

-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> StateP -> StateP
updateVar (idv, v) (lcx, gl) = case M.lookup idv gl of
                                 (Just _) -> (lcx, M.insert idv v gl)
                                 Nothing  -> case lcx of
                                               []       -> ([M.fromList [(idv, v)]], gl)
                                               (l:lcx') -> (M.insert idv v l : lcx', gl)

-- Задача 5 -----------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Add (I x) (I y)   = I $ x + y
applyOp Minus (I x) (I y) = I $ x - y
applyOp Mul (I x) (I y)   = I $ x * y
applyOp Less (I x) (I y)  = if x < y then I 1 else I 0
applyOp Equal (I x) (I y) = if x == y then I 1 else I 0
applyOp Index (A a) (I i) = I $ go a
  where
    go [] = 0
    go ((i', e):xs)
      | i' == i = e
      | otherwise = go xs
applyOp op arg1 arg2      = error $ "Неправильне використання оператора " ++ 
                              concat [show op, ": ", show arg1, ", ", show arg2]

-- Задача 6 -----------------------------------------
bindArgs :: [Id] -> [Value] -> Binding
-- Передумова: списки мають однакову довжину
bindArgs idx vx = M.fromList $ zip idx vx

-- Задача 7 -----------------------------------------
eval :: Exp -> [FunDef] -> StateP -> Value
eval (Const c) _ _ = c
eval (Var idv) _ st  = getValue idv st
eval (Cond p t f) dfx st = case eval p dfx st of
                             I 0 -> eval f dfx st
                             _   -> eval t dfx st
eval (OpApp op a1 a2) dfx st = applyOp op (eval a1 dfx st) (eval a2 dfx st)
eval (FunApp f es) dfx st@(lcx, gl) = eval ef dfx (bindArgs (initv <$> as) vs : lcx, gl)
  where
    (as, ef) = lookUp f dfx
    vs = evalArgs es dfx st
    initv (Arr idv) = idv
    initv (Int idv) = idv

evalArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
evalArgs es dfx st = (\e -> eval e dfx st) <$> es

-- Задача 8 -----------------------------------------
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeStatement (Assign idv ev) dfx _ st                 = updateVar (idv, eval ev dfx st) st
executeStatement (AssignA idv ei ev) dfx _ st             = updateVar (idv, arr') st
  where
    arr = getValue idv st
    arr' = assignArray arr (eval ei dfx st) (eval ev dfx st)
executeStatement (If p bt bf) dfx dpx st                  = case eval p dfx st of
                                                              I 0 -> executeBlock bf dfx dpx st
                                                              _   -> executeBlock bt dfx dpx st
executeStatement while@(While p b) dfx dpx st             = case eval p dfx st of
                                                              I 0 -> st
                                                              _   -> executeStatement while dfx dpx st'
  where
    st' = executeBlock b dfx dpx st
executeStatement (Call resId pid es) dfx dpx st@(lcx, gl) = case resId of
                                                              ""  -> st'
                                                              idv -> updateVar (idv, res) st'
  where
    (as, bl) = lookUp pid dpx
    vs = evalArgs es dfx st
    initv (Arr idv) = idv
    initv (Int idv) = idv
    pst@(_, gl') = executeBlock bl dfx dpx (bindArgs (initv <$> as) vs : lcx, gl)
    res = getValue "$res" pst
    st' = (lcx, gl')
executeStatement (Return e) dfx _ st = updateVar ("$res",res) st
  where
    res = eval e dfx st


executeBlock :: Block -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeBlock bs dfx dpx st = foldl (\st' s -> executeStatement s dfx dpx st') st bs

---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nНе знайдено  " ++ show x )) 
              (lookup x t)

-- Стан для тестування
sampleState :: StateP
sampleState = ([M.fromList [("x",I 5)]], M.fromList [("y",I 2),("a", listToVal [4,2,7])])

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n = Const (I n)

-- Реалізація виконання програми 
program :: Program -> StateP 
program (dvx, dfx, dpx) = 
   let initv :: VarDef -> (Id, Value)
       initv (Arr v) = (v, A [])
       initv (Int v) = (v, I 0) 
       gl = M.fromList (map initv dvx) 
   in executeStatement (Call "" "main" []) dfx dpx ([],gl)

-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- func  fib(n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1 = ("sumA1",
     ([Arr "a", Int "n"], 
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s") 
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd = ("gAdd", 
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])

