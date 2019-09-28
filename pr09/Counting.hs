module Counting where

counting :: String -> String
counting xs = show (length (lines xs), length (words xs), sum([length w | w <- words xs])) 
