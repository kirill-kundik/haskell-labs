{-# OPTIONS_GHC -Wall #-}
module Kundik07 where

import Data.Maybe (fromMaybe)

type Index = Int
data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Index, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (nodeInit, xs) env = go nodeInit
    where
        go 0 = False
        go 1 = True
        go x = go x'
            where
                (i,f,t) = lookUp x xs
                x' = if lookUp i env then t else f


lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nНе знайдено  " ++ show x ))
              (lookup x t)
-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Index, Bool)]]
sat (_, []) = []
sat bdd@(start, _) = go 1
    where
        go node = if node == start then [[]]
            else do
                node' <- lookUpBddByChildNode bdd node
                map (snd node':) (go (fst node'))

lookUpBddByChildNode :: BDD -> NodeId -> [(NodeId, (Index, Bool))]
lookUpBddByChildNode (_,nodes) node = go nodes
        where
            go [] = []
            go ((node', (i, f, t)):xs) = if f == node && t == node then
                                            (node', (i, False)) : (node', (i, True)) : go xs
                                         else if f == node then
                                            (node', (i, False)) : go xs
                                         else if t == node then
                                            (node', (i, True)) : go xs
                                         else go xs


-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Prim e)) = if e==True then Prim False else Prim True
simplify (Or (Prim e1) (Prim e2)) = if (e1||e2)==True then Prim True else Prim False
simplify (And (Prim e1) (Prim e2)) = if (e1&&e2)==True then Prim True else Prim False
simplify e@(Or (Prim e1) (IdRef _)) = if e1==True then Prim True else e
simplify e@(Or (IdRef _) (Prim e2)) = if e2==True then Prim True else e
simplify e@(And (Prim e1) (IdRef _)) = if e1==False then Prim False else e
simplify e@(And (IdRef _) (Prim e2)) = if e2==False then Prim False else e
simplify e = e

-- Задача 4 -----------------------------------------
restrict :: BExp -> Index -> Bool -> BExp
restrict (Not e) i v = simplify (Not (restrict e i v))
restrict (Or x y) i v = simplify (Or (restrict x i v) (restrict y i v))
restrict (And x y) i v = simplify (And (restrict x i v) (restrict y i v))
restrict (IdRef i') i v = if i' == i then Prim v else IdRef i'
restrict (Prim v') _ _ = Prim v'

-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
buildBDD :: BExp -> [Index] -> BDD
buildBDD e = buildBDD' e 2 

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim False) _ [] = (0, [])
buildBDD' (Prim True) _ [] = (1, [])
buildBDD' _ node [] = (node, [])
buildBDD' e node (x:xs) = (node,(node, (x, nodeFalse, nodeTrue)):nodesFalse ++ nodesTrue)
    where 
        xFalse = restrict e x False
        xTrue = restrict e x True
        (nodeFalse, nodesFalse) = buildBDD' xFalse (node*2) xs
        (nodeTrue, nodesTrue) = buildBDD' xTrue (node*2 + 1) xs

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
fst3, snd3, trd3 :: (a, a, a) -> a
fst3 (x, _, _) = x
snd3 (_, x, _) = x
trd3 (_, _, x) = x

buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e i = (findHead (body e i) 0, body e i)
                 where body x y = removeChecks (removeRedundant (snd $ buildBDD x y) 0) 0

findHead :: [BDDNode] -> Int -> NodeId
findHead x i = if (length (filter (\j -> ((snd3 $ snd j)==(fst (x!!i)) || (trd3 $ snd j)==(fst (x!!i)))) x) == 0)
               then fst (x!!i)
               else findHead x (i+1)

removeChecks :: [BDDNode] -> Int -> [BDDNode]
removeChecks x i | i==(length x) = x
                 | otherwise = if (snd3 $ snd(x!!i))==(trd3 $ snd(x!!i))
                               then removeChecks (replace x (fst (x!!i), trd3 $ snd(x!!i))) (i)
                               else removeChecks x (i+1)

removeRedundant :: [BDDNode] -> Int -> [BDDNode]
removeRedundant x i | i==(length x) = x
                    | otherwise = if (length (filter (\j -> equalNodes j (x!!i)) (tail x))) > 0
                                  then removeRedundant (replace x (fst (x!!i), fst (head (filter (\j -> equalNodes j (x!!i)) (tail x))))) (i)
                                  else removeRedundant x (i+1)

replace :: [BDDNode] -> (NodeId, NodeId) -> [BDDNode]
replace [] _ = []
replace ((x, (a, b, c)):ns) i@(id1, id2) | b==id1 && c==id1 = (x, (a, id2, id2)) : (replace ns i)
                                         | b==id1 = (x, (a, id2, c)) : (replace ns i)
                                         | c==id1 = (x, (a, b, id2)) : (replace ns i)
                                         | x==id1 = replace ns i
                                         | otherwise = (x, (a, b, c)) : (replace ns i)

equalNodes :: BDDNode -> BDDNode -> Bool
equalNodes (id1, (x1, y1, z1)) (id2, (x2, y2, z2)) = x1==x2 && y1==y2 && z1==z2 && id1/=id2

------------------------------------------------------
-- Приклади для тестування..

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = And (IdRef 3) (Or (IdRef 2) (And (Not (IdRef 2)) (IdRef 1)))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
bdd9 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,0,0)),(9,(3,0,1)),(5,(2,10,11)),(10,(3,0,1)),(11,(3,0,1))])



