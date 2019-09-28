{-# OPTIONS_GHC -Wall #-}
module Kundik08 where

import Data.Array

type Graph = Array Int [Int]


gr1, gr2, gr3, gr4, gr5, gr6, gr7 :: Graph
gr1 = array (0,9) [(0, []), (1,[2]), (2,[3,5]), (3, [7]), (4, [7]), (5, [8]), (6, [9]), (7, [8]), (8, [3]), (9, [])]

gr2 = array (0,9) [(0, []), (1,[2]), (2,[3,5]), (3, [7]), (4, [7]), (5, [8]), (6, [9]), (7, []), (8, [3]), (9, [])]

gr3 = array (0,4) [(0, []), (1, [2,3,4]), (2, [3,4]), (3, [4]), (4, [])]
gr4 = array (0,4) [(0, []), (1, [2,3,4]), (2, [3]), (3, [4]), (4, [])]

gr5 = array (0,9) [(0, []), (1,[2]), (2,[1,3,5]), (3, [2,7,8]), (4, [7]), (5, [2,8]), (6, [9]), (7, [3,4,8]), (8, [3,5,7]), (9, [6])]

gr6 = array (0, 3) [(0, [3]), (1, [2]), (2, [1]), (3, [])]

gr7 = array (0, 2) [(0, [1, 2]), (1, [0,2]), (2, [0,1])]
-- ������ 1 ------------------------------------------

dfs :: Graph -> [Int] -> [Int] -> [Int]
dfs _ visited [] = reverse visited
dfs graph visited (x:xs) | elem x visited = dfs graph visited xs
                         | otherwise = dfs graph (x:visited) ((graph ! x) ++ xs)

adjacent :: Graph -> Int -> [Int]
adjacent g v = g ! v

nodes :: Graph -> [Int]
nodes g = indices g

edges :: Graph-> [(Int,Int)]
edges g = [(v1,v2) | v1<-nodes g, v2 <- g!v1]

edgeIn :: Graph -> (Int,Int)-> Bool
edgeIn g(x,y) = elem y (g!x)

allWaysFromVerticeToOthers::Graph -> Int -> [Int]
allWaysFromVerticeToOthers gr n = helper [] (gr!n) where
 helper res [] = res
 helper res (x:xs) = if (elem x res) 
 then helper res xs 
 else (helper (res++[x]) (xs++(merge res (gr ! x))))

merge:: Eq a => [a] -> [a] ->[a]
merge res [] = res
merge res (x:xs) = if notElem x res then 
 x:(merge res xs) else
 merge res xs

isConnected ::Graph -> Int -> Int -> Bool
isConnected gr a b = elem b (allWaysFromVerticeToOthers gr a)

isConnected2 :: Graph -> Int -> Int -> Bool
isConnected2 gr a b = if a == b then False else not $ null (allExistingWays gr a b)

allExistingWays:: Graph -> Int -> Int ->  [[Int]]
allExistingWays gr a b  = (helper [a] (gr!(a))) where
 helper _ [] = []
 helper res (x:xs) = if x == b then [res++[x]] else 
  if (isConnected gr x b) && (notElem x res) then 
   helper (res++[x]) (gr!(x)) ++ helper res xs else 
   helper res xs

longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr x y = if isConnected gr x y == False then Nothing
 else Just(head $ reverse $ quickestSortEver $ allExistingWays gr x y) where
  quickestSortEver [] = []
  quickestSortEver (z:xs) = (quickestSortEver lesser) ++ [z] ++ (quickestSortEver greater) where
   lesser = filter (\n -> length n < length z) xs
   greater = filter (\n -> length n >= length z) xs  

-- ������ 2 -----------------------------------------  
isNoCycle :: Graph -> Bool
isNoCycle gr = all (\n -> null (allExistingWays gr n n)) (nodes gr)
   
-- ������ 3 -----------------------------------------
isTransitive :: Graph -> Bool
isTransitive gr = all (\(a, b) -> (edgeIn gr (a, b)) || not (isConnected2 gr a b)) [(a, b) | a <- nodes gr, b <- nodes gr]
   
-- ������ 4 -----------------------------------------
isGraph :: Graph -> Bool
isGraph gr = all (\(a, b) -> edgeIn gr (b, a)) (edges gr)

-- ������ 5 -----------------------------------------
shortWay :: Graph -> Int -> Int -> Maybe [Int]
shortWay gr x y = if isConnected gr x y == False then Nothing
 else Just(head $ quickestSortEver $ allExistingWays gr x y) where
  quickestSortEver [] = []
  quickestSortEver (z:xs) = (quickestSortEver lesser) ++ [z] ++ (quickestSortEver greater) where
   lesser = filter (\n -> length n < length z) xs
   greater = filter (\n -> length n >= length z) xs

-- ������ 6 -----------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = all (\(a, b) -> isConnected gr a b) [(a, b) | a <- nodes gr, b <- nodes gr] 

-- ������ 7 -----------------------------------------
components :: Graph -> [[Int]]
components = undefined

-- ������ 8 -----------------------------------------
topolSorting :: Graph -> Maybe[Int]
topolSorting = undefined