module Board.Pieces
(
    Player  (Black, White),
    Type    (King, Queen, Bishop, Knight, Rook, Pawn, Puste),
    Piece   (Piece),
    showPiece,
    readPiece,
    pieceValue,
    getType,
    enemy,
    typeValue,
    final,
    piecePlayer
) where

-- Typ gracza (czarny/biały)
data Player = Black | White deriving (Eq, Ord)
instance Show Player where
  show White = "W"
  show Black = "B"

enemy:: Player -> Player
enemy Black = White
enemy White = Black

-- Typ rodzaju figury
data Type = King | Queen | Bishop | Knight | Rook | Pawn | Puste deriving (Eq, Ord)

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
showPiece (Piece Black Pawn)   = 'P'
showPiece (Piece Black Knight) = 'N'
showPiece (Piece Black Bishop) = 'B'
showPiece (Piece Black Rook)   = 'R'
showPiece (Piece Black Queen)  = 'Q'
showPiece (Piece Black King)   = 'K'
showPiece (Piece White Pawn)   = 'p'
showPiece (Piece White Knight) = 'n'
showPiece (Piece White Bishop) = 'b'
showPiece (Piece White Rook)   = 'r'
showPiece (Piece White Queen)  = 'q'
showPiece (Piece White King)   = 'k'

readPiece :: Char -> Piece
readPiece 'P' = (Piece Black Pawn)
readPiece 'N' = (Piece Black Knight)
readPiece 'B' = (Piece Black Bishop)
readPiece 'R' = (Piece Black Rook)
readPiece 'Q' = (Piece Black Queen)
readPiece 'K' = (Piece Black King)
readPiece 'p' = (Piece White Pawn)
readPiece 'n' = (Piece White Knight)
readPiece 'b' = (Piece White Bishop)
readPiece 'r' = (Piece White Rook)
readPiece 'q' = (Piece White Queen)
readPiece 'k' = (Piece White King)

infinity = 1000::Integer
final = 900::Integer  -- kto ma mniej niz 900 przegrywa

typeValue:: Type -> Integer
typeValue Puste = 0
typeValue Pawn = 1
typeValue Knight = 4
typeValue Bishop = 4
typeValue Queen = 9
typeValue Rook = 6
typeValue King = infinity

pieceValue:: Piece -> Integer
pieceValue (Piece pl typ) = typeValue typ

piecePlayer:: Piece -> Player
piecePlayer (Piece pl typ) = pl

getType:: Piece -> Type
getType (Piece pl typ) = typ
