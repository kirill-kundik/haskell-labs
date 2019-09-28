{-# OPTIONS_GHC -Wall #-}
module Kundik01 where

-- Çàäà÷à 1 -----------------------------------------
power3 :: [Int]
power3 = [x^(3::Int) | x <- [1..]]

-- Çàäà÷à 2 -----------------------------------------
toPower3 :: [Int]
toPower3 = [3^x | x <- [1..]]

-- Çàäà÷à 3 -----------------------------------------
sumPower3 :: Int -> Int
sumPower3 n = sum (take n toPower3) 

-- Çàäà÷à 4 -----------------------------------------
sumPower :: Int -> Int -> Int
sumPower m n = sum ([m^x | x <- [(1::Int)..n]])

-- Çàäà÷à 5 -----------------------------------------

lessMe :: [Int] -> [Int]
lessMe xs = map (\x -> length (filter (\n -> n < x) xs)) xs
 
-- Çàäà÷à 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency (x:xs) = (x, (length (filter (\n -> n == x) xs)) + 1):frequency (filter (\n -> n /= x) xs)

-- Çàäà÷à 7 -----------------------------------------
hailstone :: Int -> Int
hailstone x = if x `mod` 2 == 0 then x `div` 2 else x*3 + 1 

-- Çàäà÷à 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq x | x == 1 = [x] | otherwise = x:hailSeq(hailstone x) 

-- Çàäà÷à 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1..]]

-- Çàäà÷à 10 -----------------------------------------

helpHailSeq :: [[Int]] -> Int -> Int
helpHailSeq [] y = y
helpHailSeq (x:xs) y = if y == 0 then 0 else
 if length x == y then head x else helpHailSeq xs y

firstHailSeq :: Int -> Int
firstHailSeq x = helpHailSeq allHailSeq x

