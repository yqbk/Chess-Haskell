module Board.Algorithm
(
positionList,
lista,
boardValue,
genGameTree,
prettyGameTree,
applyAll,
nextTurn,
gameComp,
printPosition,
compareSquare,
listOfTurns,
getMoveFromTurn,
posValue
)
where

import Board.Board
import Board.Fields
import Board.Pieces
import Board.Moves
import Board.Utils

---- Evaluation board ----
boardValue:: Board -> State
boardValue b = foldl (addValue b) (0,0) lista

addValue:: Board -> State -> Position -> State
addValue b (black, white) pos = case getSquarePos b pos of
                                  Just (Piece Black x) -> ((black+(posValue b Black x pos)), white)
                                  Just (Piece White x) -> (black, (white+(posValue b White x pos)))
                                  Nothing -> (black, white)

posValue:: Board -> Player -> Type -> Position -> Integer
posValue b player x pos = case x of
  Pawn -> (typeValue Pawn) + (prefPawnPos b player pos)
  otherwise -> typeValue x

evalTurn:: Turn -> Integer
evalTurn = evalBoard . fst

evalBoard::Board -> Integer
evalBoard b = let (p1,p2) = boardValue b in p1-p2

---- Correction for evaluation Pawns ----
prefPawnPos:: Board -> Player -> Position -> Integer
prefPawnPos b pl (x,y) = case pl of
   White -> toInteger(quot 3 (abs (0 - x)))
   Black -> toInteger(quot 3 (abs (8 - x)))

---------------------------------------------------------
---- Game Tree ----
---------------------------------------------------------
data GameTree = GameTree {turn::Turn, gameTree::[GameTree]} deriving (Show)

type NodeValue = (State,Board)

--type GameTree = Tree Turn

--addNode:: Tree a -> a -> Tree a
--addNode (Node a subtrees) val = Node a ((Node val []):subtrees)

genGameTree::Integer -> Turn -> GameTree
genGameTree 0 node = GameTree node []
genGameTree depth node | endGame node = GameTree node []
                       | otherwise = GameTree node (map (genGameTree (depth-1)) (nextPossibleTurn node))

endGame:: Turn -> Bool
endGame turn = value > final || value < -final
   where value = evalTurn turn

winningState::Player->Turn->Bool
winningState White turn = evalTurn turn > final --- dopisać szachmat
winningState Black turn = evalTurn turn < -final
winningState _ turn = val > final || val < -final ---- ??? up
  where val = evalTurn turn


---------------------------------------------------------
---- Minimax algorithm ----
---------------------------------------------------------

depth = 3

nextTurn::Turn -> Turn
nextTurn turn = case (genGameTree depth turn) of
                  GameTree node [] -> node
                  GameTree (_, player) xs -> snd (bestMove player (cmp player) (map (\x->(play x, turn x)) xs))
    where cmp White = (>)
          cmp Black = (<)


bestMove :: Player -> (Integer -> Integer -> Bool) -> [(Integer, Turn)] -> (Integer, Turn)
bestMove _ _ [x] = x
bestMove player cmp ((boardValue,turn):xs) | winningState player turn = (boardValue,turn)
                                           | otherwise = let (x, y) = bestMove player cmp xs in
                                                if cmp boardValue x then (boardValue,turn) else (x,y)


play::GameTree -> Integer
play (GameTree p []) = evalTurn p
play (GameTree (_, White) xs) = maximum (map play xs)
play (GameTree (_, Black) xs) = minimum (map play xs)


-------------------

printPosition::Either Turn String->String
printPosition (Left zust) = '\n':showBoard (boardToList(fst zust))
printPosition (Right s) = '\n':s


gameComp::Turn->[Either Turn String]
gameComp st | sw > final  = [Right "White wins!"]
            | sw < -final = [Right "Black wins!"]
            | otherwise = (Left st):gameComp (nextTurn st)
   where sw = evalTurn st



applyAll::a->[a->a]->a
applyAll a [] = a
applyAll a (f:xs) = applyAll (f a) xs





prettyGameTree::GameTree->String
prettyGameTree = prettyGameTree2 0
   where prettyGameTree2 x (GameTree z bs) = showBoardIndent (10*x) (boardToList(fst z)) ++
                                               ' ':show (evalTurn z) ++
                                               concatMap (prettyGameTree2 (x+1)) bs












------------------ZŁOTO--------------------------------
{-
listOfTurns:: Turn -> [Turn] -> [Turn]
listOfTurns x xs = (nextTurn x):xs

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

-}
----------------------------------------------------------------------------------------------
