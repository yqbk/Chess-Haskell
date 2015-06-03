module Board.Game
(
positionList,
lista,
boardValue
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

type GameTree = Tree NodeValue

addNode:: Tree a -> a -> Tree a
addNode (Node a subtrees) val = Node a (Node val []):subtrees
