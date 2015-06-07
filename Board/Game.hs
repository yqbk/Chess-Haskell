module Board.Game
(
positionList,
lista,
boardValue,
genGameTree
)
where

import Board.Board
import Board.Fields
import Board.Pieces
import Board.Moves


type State = (Integer, Integer)

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

genGameTree::Int -> Turn -> GameTree
genGameTree 0 node = GameTree node []
genGameTree depth node | endGame node = GameTree node []
                       | otherwise = GameTree node (map (genGameTree (depth-1)) (nextTurn node))



endGame:: Turn -> Bool
endGame st = sw > final || sw < -final
   where sw = evalState st


evalState:: Turn -> Integer
evalState = evalBoard . fst


{-
boardAnalysis::Board -> (Int,Int)
boardAnalysis = foldl addValue (0,0) . concat
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valuePiece a)
                                             | otherwise = (pw + valuePiece a, pb)
-}

evalBoard::Board -> Integer
evalBoard b = let (p1,p2) = boardValue b in p1-p2
