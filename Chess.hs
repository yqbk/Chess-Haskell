module Chess where

import Board.Moves
import Board.Board
import Board.Fields
import Board.Game
import Board.Algorithm
import Board.Pieces
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


exampleMateGame1 = putStr $ concatMap printPosition $ gameComp mate1
exampleMateGame2 = putStr $ concatMap printPosition $ gameComp mate2
start = putStr $ concatMap printPosition $ gameComp (initBoard, White)

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
  hPutStrLn stderr "Game history"
  mapM_ (hPutStrLn stderr.show) h


play :: String -> Gameplay ()
play i = do
  s<- get
  case parse parseACN "Parsing ACN error" i of
    Right acn -> (liftIO $ hPutStrLn stderr $ "actual move = " ++ (show acn)) >> put (acn:s)
    Left _ -> fail ("koniec")
  liftIO $ printHistory s
  liftIO $ putStrLn "f3f2" >> hFlush stdout


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
    Just "w" -> putStrLn "a2c4" >> hFlush stdout >> go -- białe wykonują pierwszy ruch
    Nothing -> go  -- domyślnie grają czarne
    where go = evalStateT doPlay []
