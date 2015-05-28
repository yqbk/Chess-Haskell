module Board.Moves
(
  movePos,
  deleteSquare,
  getSquare,
  updateBoard,
  move,
  getMoves,
  moveFld,
  getSquarePos,
  getPieceMoves
)
where

import Board.Board
import Board.Fields
import Board.Pieces
import Data.List (intercalate)
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V


type Turn = (Board, Player)

vertical, diagonal:: [Position]
vertical = [(0,1),(0,-1),(1,0),(-1,0)]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]


getMoves:: Type -> [Position]
getMoves King = vertical ++ diagonal
getMoves Queen = vertical ++ diagonal
getMoves Knight = [(1,2),(2,1),(-1,2),(2,-1),(-2,1),(1,-2),(-1,-2),(-2,-1)]
getMoves Bishop = diagonal
getMoves Rook = vertical
getMoves Pawn = []

direction:: Player -> Int
direction White = 1
direction Black = -1

{-
getPiece:: Board -> Position -> Piece
getPiece board pos = case getSquarePos board pos of
                      Just p -> p
                      Nothing -> undefined

                      -}
getPieceMoves:: Board -> Position -> [Position]
getPieceMoves board pos = case getSquarePos board pos of
                      Just (Piece p King) -> getMoves King
                      Just (Piece p Queen) -> getMoves Queen
                      Just (Piece p Knight) -> getMoves Knight
                      Just (Piece p Bishop) -> getMoves Bishop
                      Just (Piece p Rook) -> getMoves Rook
                      Just (Piece p Pawn) -> getMoves Pawn
                      Nothing -> []

                      {-
genPieceMoves:: Board -> Position -> [Position]
genPieceMoves b pos = case getPiece b pos of
                      King -> 1



genMoves::Board -> Position -> [Board]
genMoves b pos = case getSquarePos b pos of
                  Nothing -> []
                  Just p -> map (flip (movePos pos) b) $ genPieceMoves b pos p


genPieceMoves:: Board -> Position -> [Position]
genPieceMoves b pos = let
  name = expression
  in expression

genPieceMoves::Board->Pos->Piece->[Position]
genPieceMoves b pos (Piece Knight f) = let
  name = expression
  in expression



  [coord|v<-moves Knight, let coord = addPair pos v, notColor b f coord]


addPair::(Int,Int)->(Int,Int)->(Int,Int)
addPair (a,b) (c,d) = (a+c,b+d)
-}

{-

genPieceMoves b pos (Piece King f) = [coord|v<-moves King, let coord = addPair pos v, notColor b f coord]
genPieceMoves b pos (Piece Pawn f) = (filter (empty b) [addPair pos (direction f, 0)]) ++
                                     (filter (oppositePiece b f) (map (addPair pos) [(direction f, 1),(direction f, -1)]))
genPieceMoves b pos (Piece x f) = concatMap (iterateDirection 1 pos b f) (moves x)


notColor b f p      = inside p && not (hasColor f (getSquare b p))
empty b p           = inside p && Nothing == (getSquare b p)
oppositePiece b f p = inside p && hasColor (oppositeColor f) (getSquare b p)


-}
{-
getPossibleMoves (Piece White Pawn)   = [()]
getPossibleMoves (Piece White Knight) = [()]
getPossibleMoves (Piece White Bishop) = [()]
getPossibleMoves (Piece White Rook)   = [()]
getPossibleMoves (Piece White Queen)  = [()]
getPossibleMoves (Piece White King)   = [()]
getPossibleMoves (Piece Black Pawn)   = [()]
getPossibleMoves (Piece Black Knight) = [()]
getPossibleMoves (Piece Black Bishop) = [()]
getPossibleMoves (Piece Black Rook)   = [()]
getPossibleMoves (Piece Black Queen)  = [()]
-}


{-
getPiece:: Field -> Piece
getPiece x y


getMove:: Field -> Board -> [Field]
getMove fld board = let
  c = showSquare (getSquare board fld)
  -}

moveFld::Board -> Field -> Field -> Board
moveFld (Board b) bgn end = let
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

getSquarePos:: Board -> Position -> Square
getSquarePos (Board b) (x,y) = b ! x ! y

updateBoard:: Board -> Field -> Field -> Board
updateBoard b bgn end = deleteSquare (moveFld b bgn end) bgn

movePos:: Board -> Position -> Position -> Board
movePos (Board b) bgn (x,y) = let
  new = getSquarePos (Board b) bgn
  in Board $ b // [ (x,((b ! x) // [(y,new)]))]

move:: Board -> String -> String -> Board
move board a b = updateBoard board (strToField a) (strToField b)
