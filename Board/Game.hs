module Board.Game
(
positionList,
lista,
boardValue,
genGameTree,
prettyGameTree,
applyAll,
doMove,
gameComp,
printPosition,
compareSquare,
listOfTurns,
getMoveFromTurn
)
where

import Board.Board
import Board.Fields
import Board.Pieces
import Board.Moves


type State = (Integer, Integer)
type Move = (Position, Position)

lista = positionList [] 0 0

positionList:: [Position] -> Int -> Int -> [Position]
positionList list a b
  | a < 7 = positionList ((a,b):list) (a+1) b
  | b < 7 = positionList ((a,b):list) 0 (b+1)
  | otherwise = ((7,7):list)

addValue:: Board -> State -> Position -> State
addValue b (black, white) pos = case getSquarePos b pos of
                                  Just (Piece Black x) -> ((black+(typeValue x)), white)
                                  Just (Piece White x) -> (black, (white+(typeValue x)))
                                  Nothing -> (black, white)


boardValue:: Board -> State
boardValue b = foldl (addValue b) (0,0) lista

---------------------------------------------------------
data Tree a = Node a [Tree a] deriving (Show)

type NodeValue = (State,Board)

--type GameTree = Tree Turn
data GameTree = GameTree {turn::Turn, gameTree::[GameTree]} deriving (Show)

addNode:: Tree a -> a -> Tree a
addNode (Node a subtrees) val = Node a ((Node val []):subtrees)

genGameTree::Integer -> Turn -> GameTree
genGameTree 0 node = GameTree node []
genGameTree depth node | endGame node = GameTree node []
                       | otherwise = GameTree node (map (genGameTree (depth-1)) (nextTurn node))



endGame:: Turn -> Bool
endGame st = sw > final || sw < -final
   where sw = evalState st


evalState:: Turn -> Integer
evalState = evalBoard . fst

evalBoard::Board -> Integer
evalBoard b = let (p1,p2) = boardValue b in p1-p2
-----------------syf------------------------------------------




prettyGameTree::GameTree->String
prettyGameTree = prettyGameTree2 0
   where prettyGameTree2 x (GameTree z bs) = showBoardIndent (10*x) (boardToList(fst z)) ++
                                               ' ':show (evalState z) ++
                                               concatMap (prettyGameTree2 (x+1)) bs




play::GameTree -> Integer
play (GameTree p []) = evalState p
play (GameTree (_, White) xs) = maximum (map play xs)
play (GameTree (_, Black) xs) = minimum (map play xs)

winningState::Player->Turn->Bool
winningState White st = evalState st > final
winningState Black st = evalState st < -final

findBest :: Player -> (Integer -> Integer -> Bool) -> [(Integer, Turn)] -> (Integer, Turn)
findBest _ _ [x] = x
findBest f cmp ((x1,y1):xs) | winningState f y1 = (x1,y1)
                            | otherwise = let (x2, y2) = findBest f cmp xs in
                                             if cmp x1 x2 then (x1,y1) else (x2,y2)

depthh = 3

------------------ZŁOTO--------------------------------

listOfTurns:: Turn -> [Turn] -> [Turn]
listOfTurns x xs = (doMove x):xs

getMoveFromTurn:: [Turn] -> Move
getMoveFromTurn (a:b:xs) = getDifference (fst a) (concat $ boardToList(fst a)) (fst b) (concat $ boardToList(fst b))
getMoveFromTurn (a:xs) = getDifference (fst a) (concat $ boardToList(fst a)) initBoard (concat $ boardToList(initBoard))

findSquarePos:: Board -> Square -> [Position] -> Position
findSquarePos b sqr (x:list) | getSquarePos b x == sqr = x
                             | otherwise = findSquarePos b sqr list

getDifference:: Board -> [Square] -> Board -> [Square] -> Move
getDifference b1 (x:xs) b2 (y:ys) = case compareSquare x y of
                              True -> getDifference b1 xs b2 ys
                              False -> (findSquarePos b1 x lista, findSquarePos b2 x lista)

compareSquare:: Square -> Square -> Bool
compareSquare a b | a == b = True
                  | otherwise = False

----------------------------------------------------------------------------------------------
doMove::Turn -> Turn
doMove z = case (genGameTree depthh z) of
                  GameTree p [] -> p
                  GameTree (_, f) xs -> snd (findBest f (comp f) (map (\x->(play x, turn x)) xs))
    where comp White = (>)
          comp Black = (<)



printPosition::Either Turn String->String
printPosition (Left zust) = '\n':showBoard (boardToList(fst zust))
printPosition (Right s) = '\n':s


gameComp::Turn->[Either Turn String]
gameComp st | sw > final  = [Right "White wins!"]
            | sw < -final = [Right "Black wins!"]
            | otherwise = (Left st):gameComp (doMove st)
   where sw = evalState st



applyAll::a->[a->a]->a
applyAll a [] = a
applyAll a (f:xs) = applyAll (f a) xs
