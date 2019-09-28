breaks :: [a] -> [[[a]]]
breaks [v] = [[[v]]]
breaks (x:xs) = let xsss = breaks xs 
                    x1 = map (\(ys:yss) -> [x] : (ys:yss)) xsss
                    x2 = map (\(ys:yss) -> (x:ys):yss) xsss
                in x1 ++ x2

type Expr = [Term]
type Term = [String]

build :: String -> [Expr]
build = concatMap breaks . breaks 

evalTerm :: Term -> Int
evalTerm = product . map read

eval :: Expr -> Int
eval = sum . map evalTerm 

find :: Int -> String -> [Expr]
find v = filter ((==v).eval) . build

main :: IO()
main = do putStrLn "Hello, world!"
          putStrLn "Hello"

-- sum2 :: String -> IO()
-- sum2 s = putStrLn s1 where s1 = read (getLine s)
--                            s2 = read (getLine s) 

-- evalSum :: String -> String -> String
-- evalSum a b = case (takeInt a, takeInt b) of True -> (takeInt a, takeInt b)
-- 	      otherwise -> (takeInt a, takeInt b)

-- takeInt :: String -> Maybe Int
-- takeInt sm = let (_, sm1) = span (=='') sm
--                  (num, sm2) = span (`elem` "0123456789") sm1
--                  (_, sm3) = span (=='') sm2
--              in if null sm3 then Just (read num) else Nothing


type FilePath = String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO()

