{-# OPTIONS_GHC -Wall #-}
module Kundik02 where

-- ������ 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl1 (+) xs
  
-- ������ 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs = foldr1 (*) xs

-- ������ 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs


-- ������ 4 -----------------------------------------
insert :: Ord a => [a] -> a -> [a]
insert as a = [x | x <- as, x < a] ++ [a] ++ [x | x <- as, x >= a]

sortInsert :: [Int] -> [Int]
sortInsert xs = foldl insert [] xs

-- ������ 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = map fst $ filter (p . snd) (zip [0..] xs)

-- ������ 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = reverse (map reverse xss)

-- ������ 7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (\n -> not (elem n ['0'..'9'])) xs

-- ������ 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood xs x = length (filter (\n -> n == True) [ys x | ys <- xs])

-- ������ 9 ------------------------------------------
row :: Num a => [a] -> [a]
row xs = zipWith (+) (0:xs) (xs++[0])

trianglePas :: [[Integer]]
trianglePas = iterate (row) [1]

-- ������ 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]
