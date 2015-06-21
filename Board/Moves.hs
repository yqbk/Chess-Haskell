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
  getMovesPawn,
  jump,
  pieceJump,
  getPlayerPos,
  moveGenerator,
  friendlyPieces,
  nextPossibleTurn,
  pawnAttack,
  Turn,
  move',
  enemyPieces,
  pawnFirstMove,
  promotionTurn,
  isPromotion,
  putNew
  )
where

import Board.Board
import Board.Fields
import Board.Pieces
import Board.Utils
import Data.List (intercalate,(\\))
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V

---- Keep actual game state ----
type Turn = (Board, Player, (String,String))

---- Set move options and moves utils ----
direction:: Player -> Int
direction White = 1
direction Black = -1

vertical, diagonal:: [Position]
vertical = [(0,1),(0,-1),(1,0),(-1,0)]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]

getMoves:: Type -> [Position]
getMoves King = vertical ++ diagonal
getMoves Queen = vertical ++ diagonal
getMoves Knight = [(1,2),(2,1),(-1,2),(2,-1),(-2,1),(1,-2),(-1,-2),(-2,-1)]
getMoves Bishop = diagonal
getMoves Rook = vertical

friendlyFire:: Player -> Square -> Bool
friendlyFire _ Nothing = False
friendlyFire p1 (Just (Piece p2 x)) = p1 == p2

onBoard, notOnBoard::Position -> Bool
onBoard (a, b) = a >= 0 && b >= 0 && a <= 7 && b <= 7
notOnBoard = not . onBoard

------------------------------------------------------------------
movesOnBoard:: [Position] -> [Position] -> [Position]
movesOnBoard [] s = s
movesOnBoard (x:xs) s = case onBoard x of
                        True -> movesOnBoard xs (x:s)
                        False -> movesOnBoard xs s

movesNotFriendlyFire:: Board -> Player -> [Position] -> [Position] -> [Position]
movesNotFriendlyFire board player [] s = s
movesNotFriendlyFire board player (x:xs) s = case friendlyFire player (getSquarePos board x) of
                        False -> movesNotFriendlyFire board player xs (x:s)
                        True -> movesNotFriendlyFire board player xs s


possible:: Board -> Player -> Position -> Bool
possible board player pos = onBoard pos && not (friendlyFire player (getSquarePos board pos))

jump:: Board -> Position -> Int -> Position -> [Position]
jump b pos n new | notOnBoard destination = []
                 | otherwise = case getSquarePos b destination of
                           Nothing -> destination:jump b pos (n+1) new
                           Just (Piece pl _) -> case pl == pl2 of
                                                  True -> []
                                                  False -> [destination]
    where destination = add (multiply new n) pos
          pl2 = getPlayerPos b pos
------------------------------------------------------------------
getPieceMoves:: Board -> Position -> [Position]
getPieceMoves board pos = case getSquarePos board pos of
                      Just (Piece p King) -> movesNotFriendlyFire board p (movesOnBoard (map (add pos) (getMoves King)) []) []
                      Just (Piece p Knight) -> movesNotFriendlyFire board p (movesOnBoard (map (add pos) (getMoves Knight)) []) []
                      Just (Piece p Pawn) -> movesNotFriendlyFire board p (movesOnBoard ((correctPawnMove board pos (map (add pos) (getMovesPawn board pos p)) [])++(pawnAttack board p pos)) []) []
                      Just (Piece p x) -> concatMap (jump board pos 1) (getMoves x)
                      Nothing -> []

pawnFirstMove :: Position -> Player -> Bool
pawnFirstMove (x,y) pl = case pl of
                        White -> x == 1
                        Black -> x == 6

--pawnEnPassant:: Position -> Player -> Bool
--pawnEnPassant (x,y) pl



pawnAttack:: Board -> Player -> Position -> [Position]
pawnAttack b pl pos = case pl of
                      Black -> concatMap (checkPawnAttack b pos) ((-1,-1):(-1,1):[])
                      White -> concatMap (checkPawnAttack b pos) ((1,-1):(1,1):[])


checkPawnAttack:: Board -> Position -> Position -> [Position]
checkPawnAttack b pos new | notOnBoard destination = []
                          | otherwise = case getSquarePos b destination of
                             Nothing -> []
                             Just (Piece pl _) -> case pl == pl2 of
                                                  True -> []
                                                  False -> [destination]
              where destination = add new pos
                    pl2 = getPlayerPos b pos

correctPawnMove:: Board -> Position -> [Position] -> [Position] ->[Position]
correctPawnMove b pos (x:xs) y = case getSquarePos b x of
                              Nothing -> correctPawnMove b pos xs (x:y)
                              otherwise -> correctPawnMove b pos xs y
correctPawnMove b pos [] y = y


getMovesPawn :: Board -> Position -> Player -> [Position]
getMovesPawn b pos pl = case pawnFirstMove pos pl of
                  True -> case pl of
                          Black -> ((-1,0):(-2,0):[])
                          White -> ((1,0):(2,0):[])
                  False -> case pl of
                          Black -> ((-1,0):[])
                          White -> ((1,0):[])

promotion:: Board -> Player -> Position -> Board
promotion b pl (x,y) = case isPromotion b (x,y) of
  True -> putNew b pl (x,y)
  otherwise -> b

isPromotion:: Board -> Position -> Bool
isPromotion b (x,y) = case getSquarePos b (x,y) of
                      Just (Piece _ Pawn) ->  (x==0)||(x==7)
                      otherwise -> False

putNew:: Board -> Player -> Position -> Board
putNew (Board b) pl (x,y) = Board $ b // [ (x,((b ! x) // [(y,(Just (Piece pl Queen)))]))]

promotionTurn::  Turn -> Bool
promotionTurn (b,pl,mv) = or $ map (isPromotion b) lista

------------------------------------------------------------------
deleteSquare:: Board -> Field -> Board
deleteSquare (Board b) fld = let
  (x,y) = fieldToPos fld
  in Board $ b // [ (x,((b ! x) // [(y,Nothing)]))]

deletePos:: Board -> Position -> Board
deletePos (Board b) (x,y) = Board $ b // [ (x,((b ! x) // [(y,Nothing)]))]

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

moveFld::Board -> Field -> Field -> Board
moveFld (Board b) bgn end = let
  new = getSquare (Board b) bgn
  (x,y) = fieldToPos end
  in Board $ b // [ (x,((b ! x) // [(y,new)]))]

movePos:: Board -> Position -> Position -> Board
movePos (Board b) bgn (x,y) = let
  new = getSquarePos (Board b) bgn
  in Board $ b // [ (x,((b ! x) // [(y,new)]))]

move':: Board -> String -> Board
move' board (str) = let
  halfStr = take 2 str
  a = strToPos halfStr
  b = strToPos (take 2 (str \\ halfStr))
  in move board a b


move:: Board -> Position -> Position -> Board
move board a b | isPromotion (deletePos (movePos board a b) a) b = putNew (deletePos (movePos board a b) a) pl b
               | otherwise = deletePos (movePos board a b) a
  where
    Just (Piece pl x) = getSquarePos board a


---------
pieceJump b pos (Piece x f) = concatMap (jump b pos 1) (getMoves f)

empty b p = onBoard p && Nothing == (getSquarePos b p)

------------------------------------------------------------------
moveGenerator:: Board -> Position -> [Board]
moveGenerator b pos = case getSquarePos b pos of
                      Nothing -> []
                      Just piece -> map (move b pos) $ getPieceMoves b pos

getMove::Board -> Position -> [Position]
getMove b pos = case getSquarePos b pos of
                      Nothing -> []
                      Just piece ->  getPieceMoves b pos

friendlyPieces:: Board -> Player -> [Position]
friendlyPieces b pl = [(x,y)|x<-[0..7], y<-[0..7], friendlyFire pl (getSquarePos b (x,y))]

enemyPieces:: Board -> Player -> [Position]
enemyPieces b pl = [(x,y)|x<-[0..7], y<-[0..7], not(friendlyFire pl (getSquarePos b (x,y))), not(empty b (x,y))]

nextPossibleTurn:: Turn -> [Turn]
nextPossibleTurn (b,pl,(_,_)) = [(newBoard,enemy pl,(posToStr enemyPos, posToStr newPos))|enemyPos<-enemyPieces b pl, newBoard<-moveGenerator b enemyPos, newPos<- getMove b enemyPos]
