module Main where

import Board.Moves
import Board.Board
import Board.Fields
import Board.Algorithm
import Board.Pieces
import Board.Utils
import Data.List (intercalate)
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V
import System.Environment
import Control.Monad.State
import Text.ParserCombinators.Parsec
import System.IO
import Data.Maybe

type Move = Board->Board
type Game = [Move]

-------------------------------------------------------
--------------------- Run Game ------------------------
start = putStr $ concatMap printPosition $ gameComp (initBoard, White, zeroMove)
-------------------------------------------------------

---- Game Utils ----
printPosition::Either Turn String->String
printPosition (Left zust) = '\n':showBoard (boardToList(fst' zust))
printPosition (Right s) = '\n':s

gameComp::Turn->[Either Turn String]
gameComp st | sw > final  = [Right "Black wins!"]
            | sw < -final = [Right "White wins!"]
            | otherwise = (Left st):gameComp (nextTurn st)
   where sw = evalTurn st

playGame::Game->Board
playGame = applyAll initBoard

listToBoard:: [[Maybe Piece]] -> Board
listToBoard list = Board $ fromList $ map fromList $ concat [list]

---- Parser ----
parseLetter :: Parser Char
parseLetter = oneOf "abcdefgh"

parseNumber :: Parser Char
parseNumber = oneOf "12345678"

parsePromotion :: Parser Char
parsePromotion = oneOf "qrbn"

newtype ACN = ACN (Char,Char,Char,Char,Char)

instance Show ACN where
  show (ACN (a,b,c,d,y)) = a:b:c:d:y:[]

acnToStr:: ACN -> String
acnToStr (ACN (a,b,c,d,y)) = a:b:c:d:y:[]

strToACN:: String -> ACN
strToACN (a:b:c:d:y:[]) = (ACN (a,b,c,d,y))

parseACN :: Parser ACN
parseACN = do
          x1 <- parseLetter
          y1 <- parseNumber
          x2 <- parseLetter
          y2 <- parseNumber
          prm <- option ' ' parsePromotion
          return $ ACN (x1,y1,x2,y2,prm)

---- Gameplay ----
type Gameplay a = StateT ([ACN]) IO a

printHistory :: Show a => [a] -> IO ()
printHistory h =  do
  hPutStrLn stderr "\n--------------\n--Game history--"
  mapM_ (hPutStrLn stderr.show) h
  hPutStrLn stderr "----------------\n"


doPlay :: Gameplay ()
doPlay = liftIO getContents >>= (mapM_ (play)) . lines


play ::  String -> Gameplay ()
play i = do
  --enemy <- hGetLine stdout
  s<- get
  case parse parseACN "Parsing ACN error" i of
    Right acn -> put (acn:s)
    Left _ -> fail ("koniec")
  s' <- get
  let b = actualBoard (map acnToStr (reverse s')) initBoard "a1a1"
  let pos = acnToPos (head s')
  let pl = getPlayer (fst b) pos
  let (x1,x2) = lst $ nextTurn (fst b, pl, zeroMove)
  case parse parseACN "Parsing ACN error" (x1++x2) of
    Right acn -> put (acn:s')
    Left _ -> fail ("koniec")
  liftIO $ putStrLn (responseMove pl (fst b) ) >> hFlush stdout
  --liftIO $ printHistory s'

---- Gameplay Utils ----
whoPlays:: String -> Board ->  Player
whoPlays (x1:x2:x3:x4:[]) b = getPlayerPos b $ strToPos $ x3:x4:[]

actualBoard:: [String] -> Board -> String -> (Board,String)
actualBoard [] b str = (b,str)
actualBoard (x:xs) b str = actualBoard xs (move' b x) x

responseMove:: Player -> Board -> String
responseMove pl board = let
  (_,_,(a,b)) = nextTurn (board, pl, zeroMove)
  in a++b

getPlayer:: Board -> Position -> Player
getPlayer b pos = case getSquarePos b pos of
    Just (Piece pl x) -> pl
    otherwise -> Black

acnToPos:: ACN -> Position
acnToPos acn = strToPos $ getStrAcnPos $ acnToStr acn

getStrAcnPos:: String -> String
getStrAcnPos (a:b:c:d:y:[]) = c:d:[]


----------------------
-------- Main --------
----------------------
main :: IO ()
main = do
  args <- getArgs
  case (listToMaybe args) of
    Just "b" -> go
    Just "w" -> putStrLn "d2d4" >> hFlush stdout >> go -- białe wykonują pierwszy ruch
    Nothing -> putStrLn "d7d5" >> hFlush stdout >> go  -- domyślnie grają czarne
    where go = evalStateT doPlay []
