module Board.Utils
(
State,
positionList,
lista,
fst',
lst,
zeroMove
)
where

import Board.Board
import Board.Fields
import Board.Pieces
import Board.Moves



type State = (Integer, Integer)
--type Move = (Position, Position)

---- list of all possible positions on board ----
lista = positionList [] 0 0

zeroMove = ("a1","a1")

positionList:: [Position] -> Int -> Int -> [Position]
positionList list a b
  | a < 7 = positionList ((a,b):list) (a+1) b
  | b < 7 = positionList ((a,b):list) 0 (b+1)
  | otherwise = ((7,7):list)


fst':: (a,b,c) -> a
fst' (a,b,c) = a

lst :: (a,b,c) -> c
lst (a,b,c) = c

----------------------------------------------------------------------------------------------
