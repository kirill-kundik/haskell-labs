module Practice10 where 

subseqs :: [Int] -> [[Int]]
allEdges :: [Int] -> [(Int, Int)]
-- isClique :: Graph -> [Int] -> Bool
-- cliqueNum :: Graph -> Int

subseqs [] = [[]]
subseqs (x:xs) = let xss subseqs xs 
 in map (x++) xss