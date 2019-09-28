{-# OPTIONS_GHC -Wall #-}
module Kundik09 where

import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify (Plus a) = Seq (simplify a) (Rep (simplify a))
simplify (Opt a) = Alt (simplify a) Null
simplify (Seq a b) = Seq (simplify a) (simplify b)
simplify (Alt a b) = Alt (simplify a) (simplify b)
simplify (Rep a) = Rep (simplify a)
simplify (Term a) = Term a
simplify Null = Null

-- Задача 2 -----------------------------------------
startState     :: Automation -> State
terminalStates :: Automation -> [State]
transitions    :: Automation -> [Transition] 

startState (a, _, _) = a
terminalStates (_, b, _) = b
transitions (_, _, c) = c 

-- Задача 3 -----------------------------------------
isTerminal :: State -> Automation -> Bool 
isTerminal a aut = elem a (terminalStates aut)

-- Задача 4 -----------------------------------------
checkTransition :: State -> Transition -> Bool
checkTransition a (b, _, _) = a == b

transitionsFrom :: State -> Automation -> [Transition]
transitionsFrom a aut = filter (\t -> checkTransition a t) (transitions aut)

-- Задача 5 -----------------------------------------
getLabel :: Transition -> Label
getLabel (_, _, c) = c

label :: [Transition] -> [Label]
label tr = map (getLabel) (filter (\t -> (getLabel t) /= Eps) tr)

labels :: [Transition] -> [Label]
labels tr = nub (label tr)  

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

getSnd :: Transition -> State
getSnd (_, s, _) = s

stStep aut st lab = map (getSnd) (filter (\t -> (getLabel t) == lab) (transitionsFrom st aut))
setStep _ [] _ = []
setStep aut (st:sts) lab = nub $ (stStep aut st lab) ++ (setStep aut sts lab) 

closure aut sts = nub (closure' aut sts [])

closure' :: Automation -> [State] -> [State] -> [State]
closure' _ [] _ = []
closure' aut sts reached = let curr = setStep aut sts Eps
                               newOne = (filter (\n -> notElem n reached) curr)
                           in  curr++(closure' aut newOne (curr++reached))

-- Задача 7 -----------------------------------------
accepts' :: Automation -> [Char] -> [State] -> Bool
accepts' aut [] st = any (\s -> isTerminal s aut) st
accepts' _ _ [] = False
accepts' aut (s:ss) currStates = let avalStates = closure aut currStates
                                     currAvalState = setStep aut avalStates (C s)
                                 in  accepts' aut ss currAvalState 

accepts :: Automation -> String -> Bool
accepts aut s = accepts' aut s [startState aut] 

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null start finish aval = ([(start, finish, Eps)], aval)
make (Term a) start finish aval = ([(start, finish, C a)], aval)
make (Seq ra rb) start finish aval = let (tr1, int1) = make ra start aval (aval+2)
                                         (tr2, int2) = make rb (aval+1) finish int1
                                     in (tr1++tr2++[(aval, aval+1, Eps)], int2)
make (Alt ra rb) start finish aval = let (tr1, int1) = make ra aval (aval+1) (aval+4)
                                         (tr2, int2) = make rb (aval+2) (aval+3) int1
                                     in (tr1++tr2++[(start, aval, Eps), (aval+1, finish, Eps), (start, aval+2, Eps), (aval+3, finish, Eps)], int2)
make (Rep re) start finish aval = let (tr1, int1) = make re aval (aval+1) (aval+2)
                                  in (tr1++[(start, aval, Eps), (aval+1, aval, Eps), (aval+1, finish, Eps), (start, finish, Eps)], int1)
make _ _ _ _ = undefined
-- Задача 9 -----------------------------------------
getFrontier :: State -> Automation -> [Transition]
getFrontier s aut = let spisok = nub $ s:(closure aut [s])
                        newSpisok = filter (\t -> getLabel t /= Eps) (concatMap (\y -> transitionsFrom y aut) spisok)
                        newOnlyTerminal = filter (\n -> isTerminal n aut) spisok
                        phantoms = map (\x -> (x, x, Eps)) newOnlyTerminal
                    in phantoms ++ newSpisok

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions ts = let spisokLabels = labels ts
                          makeStatesOneLabel l = sort $ nub $ map (\(_, s, _) -> s) (filter (\t -> getLabel t == l) ts)
                      in  map (\ls -> (ls, makeStatesOneLabel ls)) spisokLabels

makeDA' :: Automation -> [State] -> [MetaState] -> [MetaTransition] 
                   -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigure, re1, re2, re3, re4, re5 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Alt (Term 'a') Null) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )