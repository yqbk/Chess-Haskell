module Board.Pieces
(
    Player  (Black, White),
    Type    (King, Queen, Bishop, Knight, Rook, Pawn),
    Piece   (Piece),
    showPiece,
    readPiece,
    pieceValue,
    getType
) where

-- Typ gracza (czarny/biały)
data Player = Black | White deriving (Eq, Ord, Show)

-- Typ rodzaju figury
data Type = King | Queen | Bishop | Knight | Rook | Pawn deriving (Eq, Ord, Show)

-- Pionek -> połącznie figury i jej koloru
data Piece = Piece Player Type deriving (Eq, Ord)

-- Wypisywanie pionków
instance Show Piece where
  show (Piece Black King) = "K"
  show (Piece Black Queen) = "Q"
  show (Piece Black Bishop) = "B"
  show (Piece Black Knight) = "N"
  show (Piece Black Rook) = "R"
  show (Piece Black Pawn) = "P"
  show (Piece White King) = "k"
  show (Piece White Queen) = "q"
  show (Piece White Bishop) = "b"
  show (Piece White Knight) = "n"
  show (Piece White Rook) = "r"
  show (Piece White Pawn) = "p"

showPiece :: Piece -> Char
showPiece (Piece White Pawn)   = 'P'
showPiece (Piece White Knight) = 'N'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook)   = 'R'
showPiece (Piece White Queen)  = 'Q'
showPiece (Piece White King)   = 'K'
showPiece (Piece Black Pawn)   = 'p'
showPiece (Piece Black Knight) = 'n'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook)   = 'r'
showPiece (Piece Black Queen)  = 'q'
showPiece (Piece Black King)   = 'k'

readPiece :: Char -> Piece
readPiece 'P' = (Piece White Pawn)
readPiece 'N' = (Piece White Knight)
readPiece 'B' = (Piece White Bishop)
readPiece 'R' = (Piece White Rook)
readPiece 'Q' = (Piece White Queen)
readPiece 'K' = (Piece White King)
readPiece 'p' = (Piece Black Pawn)
readPiece 'n' = (Piece Black Knight)
readPiece 'b' = (Piece Black Bishop)
readPiece 'r' = (Piece Black Rook)
readPiece 'q' = (Piece Black Queen)
readPiece 'k' = (Piece Black King)

typeValue:: Type -> Integer
typeValue Pawn = 1
typeValue Knight = 3
typeValue Bishop = 3
typeValue Queen = 9
typeValue Rook = 5
typeValue King = 1000

pieceValue:: Piece -> Integer
pieceValue (Piece pl typ) = typeValue typ

getType:: Piece -> Type
getType (Piece pl typ) = typ
