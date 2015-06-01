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
  getPieceMoves,
  movesOnBoard,
  onBoard,
  movesNotFriendlyFire,
  friendlyFire,
  add,
  getMovesPawn,
  jump,
  pieceJump,
  getPlayerPos
)
where

import Board.Board
import Board.Fields
import Board.Pieces
import Data.List (intercalate)
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V


type Turn = (Board, Player)

direction:: Player -> Int
direction White = -1
direction Black = 1

vertical, diagonal:: [Position]
vertical = [(0,1),(0,-1),(1,0),(-1,0)]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]

getMoves:: Type -> [Position]
getMoves King = vertical ++ diagonal
getMoves Queen = vertical ++ diagonal
getMoves Knight = [(1,2),(2,1),(-1,2),(2,-1),(-2,1),(1,-2),(-1,-2),(-2,-1)]
getMoves Bishop = diagonal
getMoves Rook = vertical

{-
iterateDirection::Int->Position->Board->Type->Position->[Position]
iterateDirection n pos b f r | notOnBoard aimsAt = []
                             | otherwise = case getSquare b posToField aimsAt of
                                             Nothing -> aimsAt:iterateDirection (n+1) pos b f r
                                             Just (Piece _ f2) -> if f==f2 then [] else [aimsAt]
   where aimsAt = add (mulitply r n) pos
-}

mulitply:: Position -> Int -> Position
mulitply (a,b) x = (a*x,b*x)

add:: Position -> Position -> Position
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

friendlyFire:: Player -> Square -> Bool
friendlyFire _ Nothing = False
friendlyFire p1 (Just (Piece p2 x)) = p1 == p2

onBoard, notOnBoard::Position -> Bool
onBoard (a, b) = a >= 0 && b >= 0 && a <= 7 && b <= 7
notOnBoard = not . onBoard

------------------------------------------------------------------
movesOnBoard:: [Position] -> [Position] -> [Position]
movesOnBoard (x:xs) s = case onBoard x of
                        True -> movesOnBoard xs (x:s)
                        False -> movesOnBoard xs s
movesOnBoard [] s = s

movesNotFriendlyFire:: Board -> Player -> [Position] -> [Position] -> [Position]
movesNotFriendlyFire board player (x:xs) s = case friendlyFire player (getSquarePos board x) of
                        False -> movesNotFriendlyFire board player xs (x:s)
                        True -> movesNotFriendlyFire board player xs s
movesNotFriendlyFire board player [] s = s

possible:: Board -> Player -> Position -> Bool
possible board player pos = onBoard pos && not (friendlyFire player (getSquarePos board pos))

------------------------------------------------------------------
getPieceMoves:: Board -> Position -> [Position]
getPieceMoves board pos = case getSquarePos board pos of
                      Just (Piece p King) -> movesNotFriendlyFire board p (movesOnBoard (map (add pos) (getMoves King)) []) []
                      Just (Piece p Knight) -> movesNotFriendlyFire board p (movesOnBoard (map (add pos) (getMoves Knight)) []) []
                      Just (Piece p Pawn) -> movesNotFriendlyFire board p (movesOnBoard (map (add pos) (getMovesPawn pos p)) []) []
                      Just (Piece p x) -> concatMap (jump board pos 1) (getMoves x)
                      Nothing -> []

pawnFirstMove :: Position -> Player -> Bool
pawnFirstMove (x,y) pl = case pl of
                        White -> x == 1
                        Black -> x == 6

getMovesPawn :: Position -> Player -> [Position]
getMovesPawn pos pl = case pawnFirstMove pos pl of
                  True -> case pl of
                          Black -> [(-1,0),(-2,0)]
                          White -> [(1,0),(2,0)]
                  False -> case pl of
                          Black -> [(-1,0)]
                          White -> [(1,0)]

jump:: Board -> Position -> Int -> Position -> [Position]
jump b pos n new | notOnBoard destination = []
                 | otherwise = case getSquarePos b destination of
                           Nothing -> destination:jump b pos (n+1) new
                           Just (Piece pl _) -> case pl == pl2 of
                                                  True -> []
                                                  False -> [destination]
    where destination = add (mulitply new n) pos
          pl2 = getPlayerPos b pos

------------------------------------------------------------------
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

getPlayerPos:: Board -> Position -> Player
getPlayerPos b pos = case getSquarePos b pos of
                    Just (Piece pl _ ) -> pl

------------------------------------------------------------------
updateBoard:: Board -> Field -> Field -> Board
updateBoard b bgn end = deleteSquare (moveFld b bgn end) bgn

movePos:: Board -> Position -> Position -> Board
movePos (Board b) bgn (x,y) = let
  new = getSquarePos (Board b) bgn
  in Board $ b // [ (x,((b ! x) // [(y,new)]))]

move:: Board -> String -> String -> Board
move board a b = updateBoard board (strToField a) (strToField b)

---------
pieceJump b pos (Piece x f) = concatMap (jump b pos 1) (getMoves f)


--getPiecePossibleMoves:: Board -> Position -> [Position]

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
