module Board.Algorithm
(
positionList,
lista,
boardValue,
genGameTree,
nextTurn,
posValue,
evalTurn,
chooseBestBranch,
evalBranch,
GameTree
)
where

import Board.Board
import Board.Fields
import Board.Pieces
import Board.Moves
import Board.Utils

---- Evaluation board ----
boardValue:: Board -> State
boardValue b = foldl (addValue b) (0,0) lista

addValue:: Board -> State -> Position -> State
addValue b (white, black) pos = case getSquarePos b pos of
                                  Just (Piece Black x) -> (white, (black+(posValue b Black x pos)))
                                  Just (Piece White x) -> ((white+(posValue b White x pos)), black)
                                  Nothing -> (white, black)

posValue:: Board -> Player -> Type -> Position -> Integer
posValue b player x pos = case x of
  Pawn -> (typeValue Pawn) + (prefPawnPos b player pos)
  otherwise -> typeValue x

evalTurn:: Turn -> Integer
evalTurn = evalBoard . fst'

evalBoard::Board -> Integer
evalBoard b = let (p1,p2) = boardValue b in p1-p2

---- Correction for evaluation Pawns ----
prefPawnPos:: Board -> Player -> Position -> Integer
prefPawnPos b pl (x,y) = case pl of
   White -> maximum ((toInteger(quot (abs (0 - x)) 2)):1:[])
   Black -> maximum ((toInteger(quot (abs (7 - x)) 2)):1:[])

---------------------------------------------------------
---- Game Tree ----
---------------------------------------------------------

data GameTree = GameTree {turn::Turn, gameTree::[GameTree]} deriving (Show)

genGameTree::Integer -> Turn -> GameTree
genGameTree 0 node = GameTree node []
genGameTree depth node | endGame node = GameTree node []
                       | otherwise = GameTree node (map (genGameTree (depth-1)) (nextPossibleTurn node))

endGame:: Turn -> Bool
endGame turn = value > final || value < -final
   where value = evalTurn turn

winningState::Player->Turn->Bool
winningState White turn = evalTurn turn > final --- dopisać szachmat
winningState Black turn = evalTurn turn < -final
--winningState _ turn = val > final || val < -final ---- ??? up
  --where val = evalTurn turn


---------------------------------------------------------
---- Minimax algorithm ----
---------------------------------------------------------

depth = 3

{-


play::GameTree->Integer
play (GameTree p []) = evalTurn p
play (GameTree (White,_) xs) = maximum (map play xs)
play (GameTree (Black,_) xs) = minimum (map play xs)

nextTurn::Turn->Turn
nextTurn z = case (genGameTree depth z) of
                  GameTree p [] -> p
                  GameTree (f, _) xs -> snd (findBest f (comp f) (map (\x->(play x, state x)) xs))
    where comp White = (>)
          comp Black = (<)

findBest :: PieceColor -> (Integer -> Integer -> Bool) -> [(Integer, Turn)] -> (Integer, Turn)
findBest _ _ [x] = x
findBest f cmp ((x1,y1):xs) | winningState f y1 = (x1,y1)
                            | otherwise = let (x2, y2) = findBest f cmp xs in
                                             if cmp x1 x2 then (x1,y1) else (x2,y2)

                                             -}



                                             ----CO PODAC W FIND PATH



cmp:: Player -> (Integer -> Integer -> Bool)
cmp White = (>)
cmp Black = (<)

originPath:: Player -> GameTree -> (Integer,GameTree)
originPath pl tree = findPath pl tree tree

nextTurn:: Turn -> Turn
nextTurn (b,pl,pos) = let
    branches = getBranches (genGameTree depth (b,pl,pos)) []
    tree = genGameTree depth (b,pl,pos)
    firstBranch = head branches
    firstBranchValue = evalTurn (turn firstBranch)
  in turn (getBest (map (originPath pl) branches) (cmp $ enemy pl) (firstBranchValue, firstBranch))


getBest:: [(Integer,GameTree)] -> (Integer -> Integer -> Bool) -> (Integer,GameTree) -> GameTree
getBest [] operator (val,tree) = tree
getBest ((actualVal, node):xs) operator (val,tree) = case operator actualVal val of
  True -> getBest xs operator (actualVal, node)
  otherwise -> getBest xs operator (val,tree)



--bestTurnValue::Turn -> Integer
--bestTurnValue (b,pl) = case pl of
--  White -> maximum $ map fst (map (findPath (genGameTree depth (b,pl))) (getBranches (genGameTree depth (b,pl)) []))
--  Black -> minimum $ map fst (map (findPath (genGameTree depth (b,pl))) (getBranches (genGameTree depth (b,pl)) []))


--findPathTable:: [GameTree] -> [(Integer,GameTree)] -> [(Integer,GameTree)]
--findPathTable (x:xs) ys = findPathTable xs (x:(findPath x ))



findPath:: Player -> GameTree -> GameTree -> (Integer,GameTree)
findPath p origin (GameTree node []) = (evalTurn node, origin)
findPath p origin actual@(GameTree (b, pl,_) xs) | pl == p   = findPath p origin (chooseBestBranch (cmp $ enemy p) (actualVal actual) (evalBranch actual []))
                                               | otherwise = (bestVal, origin)
    where
          actualVal (GameTree node xs) = (evalTurn node, GameTree node xs)
          branches = getBranches actual []
          firstBranch = head branches
          firstBranchValue = evalTurn (turn firstBranch)
          best = getBest (map (findPath p origin) (branches)) (cmp p) (firstBranchValue, firstBranch)
          bestVal = evalTurn (turn best)


getBranches:: GameTree -> [GameTree] -> [GameTree]
getBranches (GameTree node (x:xs)) ys = getBranches (GameTree node xs) (x:ys)
getBranches (GameTree node []) ys = ys

evalBranch::GameTree -> [(Integer,GameTree)] -> [(Integer,GameTree)]
evalBranch (GameTree node (x:xs)) ys = evalBranch (GameTree node xs) ((evalTurn (turn x), x):ys)
evalBranch (GameTree node []) ys = ys

chooseBestBranch:: (Integer -> Integer -> Bool) -> (Integer,GameTree) -> [(Integer,GameTree)] -> GameTree
chooseBestBranch cmp (val,node) ((maxVal, newNode):xs)  = case cmp val maxVal of
                      True -> chooseBestBranch cmp (val, node) xs
                      otherwise -> chooseBestBranch cmp (maxVal, newNode) xs
chooseBestBranch cmp (val,node) [] = node
