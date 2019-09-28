{-# OPTIONS_GHC -Wall #-}
module Kundik03 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- ������� 2-3-������!!!
                   deriving (Eq, Show)

-- ������ 1 -----------------------------------------
getNode::BinTree a -> a
getNode EmptyB = error "No Node for Empty"
getNode (Node a _ _) = a

getLeft::BinTree a -> BinTree a
getLeft EmptyB = error "No child for Empty"
getLeft (Node _ x _) = x

getRight::BinTree a -> BinTree a
getRight EmptyB = error "No child for Empty"
getRight (Node _ _ x) = x

isEmpty :: BinTree a -> Bool
isEmpty EmptyB = True
isEmpty _ = False

isSearch :: (Ord a) => BinTree a -> Bool
isSearch a
    |isEmpty a = True
    |isEmpty (getLeft a) && isEmpty (getRight a) = True
    |isEmpty (getLeft a) = getNode a < getNode (getRight a) && isSearch (getLeft a) && isSearch (getRight a)
    |isEmpty (getRight a) = getNode a > getNode (getLeft a) && isSearch (getLeft a) && isSearch (getRight a)
    |otherwise = getNode a > getNode (getLeft a) && getNode a < getNode (getRight a) && isSearch (getLeft a) && isSearch (getRight a)

-- ������ 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch a x 
    |isEmpty a = False
    |getNode a == x = True
    |getNode a < x = elemSearch (getRight a) x
    |otherwise = elemSearch (getLeft a) x

-- ������ 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch tr x 
    |isEmpty tr = (Node x EmptyB EmptyB)
    |x < getNode tr = (Node (getNode tr) (insSearch (getLeft tr) x) (getRight tr))
    |x > getNode tr = (Node (getNode tr) (getLeft tr) (insSearch (getRight tr) x))
    |otherwise = tr

-- ������ 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch tr x | isEmpty tr = tr
    |x < getNode tr = (Node (getNode tr) (delSearch (getLeft tr) x) (getRight tr))
    |x > getNode tr = (Node (getNode tr) (getLeft tr) (delSearch (getRight tr) x))
    |x == getNode tr && isEmpty (getLeft tr) && isEmpty (getRight tr) = EmptyB
    |x == getNode tr && isEmpty (getLeft tr) = getRight tr
    |x == getNode tr && isEmpty (getRight tr) = getLeft tr
    |otherwise = (Node (getNode (getLeft tr)) (delSearch (getLeft tr) (getNode (getLeft tr))) (getRight tr))

-- ������ 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList a = (accumulateTreeToArray [] (foldl (insSearch) EmptyB a)) where
    accumulateTreeToArray res tr
        |isEmpty(tr) = res
        |isEmpty(getLeft tr)&&isEmpty(getRight tr) = res++[(getNode tr)]
        |isEmpty(getLeft tr) = [getNode tr]++(accumulateTreeToArray res (getRight tr))
        |isEmpty(getRight tr) = (accumulateTreeToArray res (getLeft tr))++[getNode tr]
        |otherwise = (accumulateTreeToArray res (getLeft tr))++[getNode tr]++(accumulateTreeToArray res (getRight tr))


-- ������ 6-----------------------------------------
maxTree :: Tree23 p -> p
maxTree (Leaf x) = x
maxTree (Node2 _ x Empty23) = x
maxTree (Node2 _ _ tr) = maxTree tr
maxTree (Node3 _ _ _ x Empty23) = x
maxTree (Node3 _ _ _ _ tr) = maxTree tr

minTree :: Tree23 p -> p
minTree (Leaf x) = x
minTree (Node2 Empty23 x _) = x
minTree (Node2 tl _ _) = minTree tl
minTree (Node3 Empty23 x _ _ _) = x
minTree (Node3 tl _ _ _ _) = minTree tl

isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Empty23 = True
isTree23 (Leaf _) = True
isTree23 (Node2 tl x tr) = x >= maxTree tl && x == minTree tr
isTree23 (Node3 tl x tm y tr) = x >= maxTree tl && x == minTree tm && y >= maxTree tm && y == minTree tr

-- ������ 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Empty23) _ = False
elemTree23 (Leaf a) p= a==p
elemTree23 (Node2 (a) b (c)) p
 | p < b = elemTree23 (a) p
 | p > b = elemTree23 (c) p
 | p == b = True
 | otherwise = False
elemTree23 (Node3 (a) b (c) d (e)) p
 | p < b = elemTree23 (a) p
 | p > b && p < d = elemTree23 (c) p
 | p > d = elemTree23 (e) p
 | p == b = True
 | p == d = True
 | otherwise = False

-- ������ 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 a b = toList a == toList b
    where
        toList Empty23 = []
        toList (Leaf x) = [x]
        toList (Node2 tl _ tr) = toList tl ++ toList tr
        toList (Node3 tl _ tm _ tr) = toList tl ++ toList tm ++ toList tr

-- ������ 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23  = undefined

-- isTerminal tr = True <=> ���� ���� ����� tr - ������ !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- ��������� ������� ����� � 2-3-������, 
--   ����� ����� - ����� ���� Node2 ��� Node3 � ��`��� �� (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - ��������� ������� - ���� 2-3-������ a 
--   : (a, Just (w, b)) - ��������� ������� ��� 2-3-������ a i b (w - �������� �������� � b)
--  insert v tr - ���� �������� v � ������ ������ tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - �������� �������� v � ������ tr � ����� - ����������� ����� 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - ���� �������� v � ������ tr � ������ - ������������� ����� 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  ������� ������ 
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB 
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)

---- 2-3-������
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )