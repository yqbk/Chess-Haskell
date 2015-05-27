module Board.Moves
(
  movePos,
  deleteSquare,
  getSquare
)
where

import Board.Board
{-}
getFig:: Field -> Board -> Figure
getFig (x y) board = (board !! y) !! x

getPossibleMoves:: String -> [Field]
getPossibleMoves "k" = (1 1) , (1,0) , (0,1) , (-1,-1) , (-1,0) , (0,-1) , (-1,1) , (1,-1)

checkMove:: [Field] -> board -> type -> [Field]
checkMove

getMove:: Field -> Board -> [Field]
getMove fld board = let
    (plId ico id) = getFig fld
    [possibilites] = getPossibleMoves ico
    -}

movePos::Board -> Field -> Field -> Board
movePos (Board b) bgn end = let
  new = getSquare (Board b) bgn
  (x,y) = fieldToPos end
  in Board $ b // [ (x,((b ! x) // [(y,new)]))]

deleteSquare:: Board -> Field -> Board
deleteSquare (Board b) fld = let
  (x,y) = fieldToPos fld
  in Board $ b // [ (x,((b ! x) // [(y,Nothing)]))]

getSquare:: Board -> Field -> Square
getSquare (Board b) fld = let
  (x,y) = fieldToPos fld
  in b ! x ! y
