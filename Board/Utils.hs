module Board.Utils
(
State,
positionList,
lista,
fst',
lst,
zeroMove,
multiply,
add
)
where

import Board.Board
import Board.Fields
import Board.Pieces

type State = (Integer, Integer)

---- empty move ----
zeroMove = ("a1","a1")

---- list of all possible positions on board ----
lista = positionList [] 0 0

positionList:: [Position] -> Int -> Int -> [Position]
positionList list a b
  | a < 7 = positionList ((a,b):list) (a+1) b
  | b < 7 = positionList ((a,b):list) 0 (b+1)
  | otherwise = ((7,7):list)

---- Utils ----
fst':: (a,b,c) -> a
fst' (a,b,c) = a

lst :: (a,b,c) -> c
lst (a,b,c) = c

multiply:: Position -> Int -> Position
multiply (a,b) x = (a*x,b*x)

add:: Position -> Position -> Position
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

----------------------------------------------------------------------------------------------
