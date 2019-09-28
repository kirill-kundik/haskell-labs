module Main where

import Counting
import System.Environment

main :: IO()
main = do args <- getArgs
          filesss <- if null args then getContents else readAll args
          putStrLn (counting filesss)

readAll :: [String] -> IO String
readAll [] = return ""
readAll (n:ns) = do readd <- readFile n
                    all <- readAll ns
                    return (readd ++ all)

-- python3 :: IO()
-- python3 = do putStr ">>> "
--              wrds <- getLine
--              putStr ">>> "
--              xxx <- getLine
--              putStrLn (last (words wrds))