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
