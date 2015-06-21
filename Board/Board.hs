module Board.Board
(
  Board(Board),
  Square,
  showSquare,
  readSquare,
  boardToList,
  initBoard,
  showBoard
) where

import Board.Pieces
import Data.List (intercalate)
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V

type Square = Maybe Piece
data Board = Board (Vector(Vector Square)) deriving (Eq)

---- Show Board ----
showSquare::Square->String
showSquare Nothing = ". "
showSquare (Just p) =  (showPiece p):[] ++ " "

showBoard:: [[Square]] -> String
showBoard = unlines . map (concatMap showSquare)

---- Board utils ----
readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare c   = Just (readPiece c)

boardToList :: Board -> [[Square]]
boardToList (Board l) = toList $ V.map toList l

---- Build initial Board ----
initBoard :: Board
initBoard = Board $ fromList $ map fromList $ concat [
  [ whiteRearLine, whiteFrontLine ]
  , (replicate 4 emptyLine)
  , [ blackFrontLine , blackRearLine]
  ]
  where
    whiteFrontLine   = frontLine White
    whiteRearLine    = rearLine White
    blackFrontLine   = frontLine Black
    blackRearLine    = rearLine Black
    emptyLine        = replicate 8 Nothing
    frontLine player = replicate 8 $ Just $ Piece player Pawn
    rearLine  player = map (Just . (Piece player)) [Rook, Knight ,Bishop ,Queen ,King ,Bishop ,Knight ,Rook]

---- Show Board ----
instance Show Board where
  show board = (unlines ((borderLine : boardStr) ++ [borderLine,bottomLegend]))
    where
      l                    = boardToList board
      boardStr             = zipWith showLine (reverse [1..8]) $ reverse l
      showSquare Nothing   = " "
      showSquare (Just pp) = show pp
      borderLine           = " " ++ (replicate 36 '-' )

      showLine :: Integer -> [Square] -> String
      showLine i pps       =
        (intercalate " | " $ (show i) : (map showSquare pps) ) ++ " |"
      bottomLegend         =
        (intercalate " | " $ ( " " : map (:[]) ['A'..'H'] ) ) ++ " |"
