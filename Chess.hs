module Chess where

import Board.Moves
import Board.Board
import Board.Fields
import Board.Game
import Board.Pieces
import Data.List (intercalate)
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V


type Move = Board->Board
type Game = [Move]

playGame::Game->Board
playGame = applyAll initBoard

mateBoard1 =
              [[Nothing, Nothing, Nothing, Just (Piece Black Queen), Nothing, Nothing, Just (Piece Black  King), Nothing],
               [Nothing, Nothing, Nothing, Nothing, Just (Piece Black Bishop), Nothing, Just (Piece Black  Pawn), Nothing],
               [Nothing, Nothing, Nothing, Nothing, Just (Piece Black  Pawn), Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Just (Piece Black  Pawn), Just (Piece White Pawn), Nothing, Nothing, Just (Piece White Queen)],
               [Nothing, Nothing, Nothing, Just (Piece White Pawn), Nothing, Just (Piece Black  Rook), Nothing, Nothing],
               [Nothing, Nothing, Just (Piece White Pawn), Nothing, Nothing, Nothing, Nothing, Just (Piece White Bishop)],
               [Nothing, Nothing, Just (Piece White Pawn), Nothing, Nothing, Just (Piece White Pawn), Nothing, Just (Piece White Pawn)],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Piece White King), Nothing]]

mateBoard2 =
              [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Just (Piece White Rook), Nothing, Nothing, Nothing, Just (Piece Black King), Nothing, Nothing],
               [Just (Piece White Rook), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Just (Piece White King), Nothing, Nothing]]

listToBoard:: [[Maybe Piece]] -> Board
listToBoard list = Board $ fromList $ map fromList $ concat [list]

mate1 = (listToBoard mateBoard1, White)
mate2 = (listToBoard mateBoard2, Black)

-- Falkbeer Countergambit
{-
exampleOpening = [move' "e2" "e4",
                  move' "e7" "e5",
                  move' "f2" "f4",
                  move' "d7" "d5"]
-}
--outputExampleOpening = putStr $ showBoard $ playGame exampleOpening
--exampleGame = putStr $ concatMap (("\n"++) . showBoard . snd ) $ take 10 $ iterate doMove (White,initBoard)
exampleMateGame1 = putStr $ concatMap printPosition $ gameComp mate1
exampleMateGame2 = putStr $ concatMap printPosition $ gameComp mate2
start = putStr $ concatMap printPosition $ gameComp (initBoard, White)
