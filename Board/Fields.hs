module Board.Fields
(
  Field(Field),
  Position,
  X(X1, X2, X3, X4, X5, X6, X7, X8),
  Y(A, B, C, D, E, F, G, H),
  fieldToPos,
  posToField,
  strToField,
  strToPos,
  posToStr
) where

import Control.Applicative ((<$>),(<*>))

data Field = Field X Y deriving (Eq, Show)

data X = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 deriving (Eq, Show)

data Y = A | B | C | D | E | F | G | H deriving (Eq, Show)

type Position = (Int,Int)


fieldToPos :: Field -> Position
fieldToPos (Field x y) = ((xInt x),(yInt y)) where
  xInt X1 = 0
  xInt X2 = 1
  xInt X3 = 2
  xInt X4 = 3
  xInt X5 = 4
  xInt X6 = 5
  xInt X7 = 6
  xInt X8 = 7
  yInt A = 0
  yInt B = 1
  yInt C = 2
  yInt D = 3
  yInt E = 4
  yInt F = 5
  yInt G = 6
  yInt H = 7

posToField :: Position -> Maybe Field
posToField (x,y) = Field <$> (intX x) <*> (intY y) where
  intX 0 = Just X1
  intX 1 = Just X2
  intX 2 = Just X3
  intX 3 = Just X4
  intX 4 = Just X5
  intX 5 = Just X6
  intX 6 = Just X7
  intX 7 = Just X8
  intX _ = Nothing
  intY 0 = Just A
  intY 1 = Just B
  intY 2 = Just C
  intY 3 = Just D
  intY 4 = Just E
  intY 5 = Just F
  intY 6 = Just G
  intY 7 = Just H
  intY _ = Nothing


strToField :: String -> Field
strToField [x,y] = let
  letterX 'a' = A
  letterX 'b' = B
  letterX 'c' = C
  letterX 'd' = D
  letterX 'e' = E
  letterX 'f' = F
  letterX 'g' = G
  letterX 'h' = H
  letterY '1' = X1
  letterY '2' = X2
  letterY '3' = X3
  letterY '4' = X4
  letterY '5' = X5
  letterY '6' = X6
  letterY '7' = X7
  letterY '8' = X8
  in Field (letterY y) (letterX x)


posToStr:: Position -> String
posToStr (x,y) = (getX x)++(getY y)

getX x | x == 0 = "a"
       | x == 1 = "b"
       | x == 2 = "c"
       | x == 3 = "d"
       | x == 4 = "e"
       | x == 5 = "f"
       | x == 6 = "g"
       | x == 7 = "h"

getY y | y == 0 = "1"
       | y == 1 = "2"
       | y == 2 = "3"
       | y == 3 = "4"
       | y == 4 = "5"
       | y == 5 = "6"
       | y == 6 = "7"
       | y == 7 = "8"

strToPos:: String -> Position
strToPos str = fieldToPos (strToField str)
