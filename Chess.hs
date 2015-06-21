module Main where

import Board.Moves
import Board.Board
import Board.Fields
import Board.Game
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


applyAll::a->[a->a]->a
applyAll a [] = a
applyAll a (f:xs) = applyAll (f a) xs

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

{-
prettyGameTree::GameTree->String
prettyGameTree = prettyGameTree2 0
   where prettyGameTree2 x (GameTree z bs) = showBoardIndent (10*x) (boardToList(fst z)) ++
                                               ' ':show (evalTurn z) ++
                                               concatMap (prettyGameTree2 (x+1)) bs
-}

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

mate1 = (listToBoard mateBoard1, White, zeroMove)
mate2 = (listToBoard mateBoard2, White, zeroMove)


exampleMateGame1 = putStr $ concatMap printPosition $ gameComp mate1
exampleMateGame2 = putStr $ concatMap printPosition $ gameComp mate2
start = putStr $ concatMap printPosition $ gameComp (initBoard, White, zeroMove)

-------------------------------------------------------

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

parseACN :: Parser ACN
parseACN = do
          x1 <- parseLetter
          y1 <- parseNumber
          x2 <- parseLetter
          y2 <- parseNumber
          prm <- option ' ' parsePromotion
          return $ ACN (x1,y1,x2,y2,prm)

-- stanem jest lista ruchów
type Gameplay a = StateT ([ACN]) IO a

printHistory :: Show a => [a] -> IO ()
printHistory h =  do
  hPutStrLn stderr "\n--------------\n--Game history--"
  mapM_ (hPutStrLn stderr.show) h
  --hPutStrLn stderr
  hPutStrLn stderr "----------------\n"


play :: String -> Gameplay ()
play i = do
  s<- get
  case parse parseACN "Parsing ACN error" i of
    Right acn -> put (acn:s)
    Left _ -> fail ("koniec")
  --liftIO $ printHistory s
  --(liftIO $ hPutStrLn stderr $ "\nactual move = " ++ (show acn)) >>
  liftIO $ putStrLn (responseMove $ actualBoard (map acnToStr s) initBoard) >> hFlush stdout


actualBoard:: [String] -> Board -> Board
actualBoard [] b = b
actualBoard (x:xs) b = actualBoard xs (move' b x)



responseMove:: Board -> String
responseMove board = let
  (_,_,(a,b)) = nextTurn (board, White, zeroMove)
  in a++b


doPlay :: Gameplay ()
doPlay = liftIO getContents >>= (mapM_ play) . lines


main :: IO ()
main = do
  args <- getArgs
  --progName <- getProgName
  --mapM_ putStrLn args
  --putStrLn progName
  --hPutStrLn stderr "Chess engine by Zbyszko z Bogdańca ®2015"
  --let args = ["w"]
  case (listToMaybe args) of
    Just "b" -> go
    Just "w" -> putStrLn "d7d5" >> hFlush stdout >> go -- białe wykonują pierwszy ruch
    Nothing -> go  -- domyślnie grają czarne
    where go = evalStateT doPlay []


  initialBoardStr = unlines [
  			   "rnbqkbnr"
  			  ,"........"
  			  ,"........"
  			  ,"........"
  			  ,"........"
  			  ,"........"
  			  ,"........"
  			  ,"RNBQKBNR"
  			  ]
