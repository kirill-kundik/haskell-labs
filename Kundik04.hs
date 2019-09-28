{-# OPTIONS_GHC -Wall #-}
module Kundik04 where

import Data.Char
  
type Name = String
type Attributes = [(Name, String)]
data XML  = Text String | Element Name Attributes [XML]   
         deriving (Eq, Show)
type Stack = [XML]

-- Задача 1 -----------------------------------------
skipSpace :: String -> String
skipSpace = dropWhile isSpace

-- Задача 2 -----------------------------------------
getAttr (Element n a []) = a

getAttribute :: String -> XML -> String
getAttribute _ (Text _) = ""
getAttribute _ (Element _ [] _) = "" 
getAttribute attr (Element n (curA:restA) c)
 | attr == fst curA = snd curA
 | otherwise = getAttribute attr (Element n restA c)

-- Задача 3 -----------------------------------------
getName :: XML -> String
getName (Element n _ _) = n

getChildren :: String -> XML -> [XML]
getChildren _ (Text _) = []
getChildren _ (Element _ _ []) = []
getChildren el (Element _ _ xs) = [x | x <- xs, getName x == el]  

-- Задача 4 -----------------------------------------
getChild :: String -> XML -> XML
getChild el x = if length (getChildren el x) == 0 then (Text "") else head (getChildren el x) 

-- Задача 5 -----------------------------------------
addChild :: XML -> XML -> XML
-- Передумова: другий аргумент - завжди побудований конструктором Element
addChild x (Element n a xs) = (Element n a (xs ++ [x]))   

-- Задача 6 -----------------------------------------
recursionGetValue :: XML -> String
recursionGetValue (Text a) = a
recursionGetValue (Element _ _ xs) = foldl (++) "" [recursionGetValue x | x <- xs] 

getValue :: XML -> XML
getValue x = (Text (recursionGetValue x))

-- Задача 7 -----------------------------------------
addText :: String -> Stack -> Stack
-- Передумова: Є по крайній мірі один елемент Element в стеку
addText s (x:xs) = (addChild (Text s) x):xs 

-- Задача 8 -----------------------------------------
popAndAdd :: Stack -> Stack
-- Передумова: Є по крайній мірі два елемента Elements в стеку
popAndAdd (x:y:xs) = (addChild x y):xs
 
-- Початковий елемент стеку 
sentinel :: XML
sentinel = Element "" [] []  

-- Задача 9 -----------------------------------------
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

replace :: String -> String
replace ('\"':xs) = "" ++ replace xs
replace (x:xs)       = x : replace xs
replace ""           = ""

parseAttribute :: String -> (String, String)
parseAttribute (' ':s) = parseAttribute $ skipSpace s
parseAttribute ('=':s) = parseAttribute s
parseAttribute ('\"':s) = let p = parseText [] s
                          in (fst p , snd $ parseAttribute $ snd p)
parseAttribute s@('>':_) = ([], s)
parseAttribute x = ([], x)

parseText :: String->String->(String, String)
parseText t s@('<':_) = (t, s)
parseText t ('\"':s) = (t, s)
parseText t (c:s) = parseText (t++c:[]) s
parseText t [] = (t, [])

parseAttributes :: String -> (Attributes, String)
-- Передумова: Рядок, що містить XML-атрибути, синтаксично вірний
parseAttributes [] = ([], [])
parseAttributes ('>':s) = ([], s)
parseAttributes (' ':s) = parseAttributes $ skipSpace s
parseAttributes s = let p1 = parseName s
                        p2 = parseAttribute $ snd p1
                        nxt = if null $ fst p2 then ([], snd p2) else parseAttributes $ snd p2
                    in if null $ fst p2 then ([], s) else ((fst p1, fst p2):(fst nxt), snd nxt)

-- Аналіз імені елемента/атрибута
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c1 : _)
  | isAlpha c1 = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

-- Задача 10 -----------------------------------------
parse :: String -> XML
-- Передумова: рядок, що містить XML-документ, синтаксично вірний
parse s = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML
-- parse' [] ((Element _ _ ch):_) = head ch
-- parse' ('<':'/':s) (e:(Element n x ch):stack) = let t = snd $ parseName s
--                                                 in parse' (tail t) ((Element n x (ch++e:[])):stack)
-- parse' ('<':s) stack = let p1 = parseName s
--                            p2 = parseAttributes $ snd p1
--                        in parse' (snd p2) ((Element (fst p1) (fst p2) []):stack)
-- parse' s ((Element n x ch):stack) = let p = parseText [] s
--                                     in parse' (snd p) ((Element n x (ch++(Text $ fst p):[])):stack)
-- parse' _ _ = sentinel
parse' [] ((Element _ _ (c : _)) :_)
  = c
parse' ('<' : '/' : xs) sk
  = parse' (tail $ dropWhile (/='>') xs) (popAndAdd sk)
parse' ('<' : xs) sk
  = parse' rest (Element name atts [] : sk)
  where
    (name, s)
      = parseName xs
    (atts, rest)
      = parseAttributes s
parse' s st
  = parse' s' (addText text st)
  where
    (text, s') = break (=='<') s

-----------------------------------------------------------------------
-- Деякі корисні функції перетворення в рядок і виводу
-- Функція перетворення в рядок ('show' function) для XML об'єктів
showXML :: XML -> String
showXML (Text t) = t
showXML (Element n as es)
     = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
       where
          showAtts ast = concatMap showAtt ast
          showAtt (n1, v) = " " ++ n1 ++ "=" ++ "\"" ++ v ++ "\""
-- Функція перетворення в рядок ('show' function) для списку XML об'єктів
showXMLs :: [XML] -> String
showXMLs = concatMap showXML
-- Функція виводу XML об'єкта на екран
printXML :: XML -> IO()
printXML = putStrLn . showXML

-------------------------------------------------------------------------
-- Тестові дані
-- Прості тести XML-об'єктів (без проміжків)
s1, s2, s3 :: String
s1 = "<a>A</a>"
s2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"
-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- XML-документ з Мал.1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Результат аналізу  попереднього докуменнту ('parse films')
filmsParsed :: XML
filmsParsed
  = Element "filmlist" 
            [] 
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")] 
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")] 
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]

