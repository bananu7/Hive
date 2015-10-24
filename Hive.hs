module Hive 
    ( module Hive.Board
    , module Hive.Coord
    , Board
    , Unit(..)
    , Player(..)
    , Move(..)
    , isValidMove
    , Point
    , validUnitMoves
    )
where

import Prelude hiding (lookup)
import Data.List.Split (chunksOf)
import Data.Ix (range)
import Data.Maybe (catMaybes, isNothing, isJust)
import qualified Data.Set as Set
import Debug.Trace

import Hive.Board
import Hive.Coord

data Unit = Beetle (Maybe Unit) | Ant | Spider | Grasshopper | Queen deriving (Eq, Show)
 
data Player = PlayerWhite | PlayerBlack deriving (Eq, Show)

type Point = AxialCoord
type Field = (Player, Unit)
data Move = MoveFromTo Point Point | MoveInsert Point Field

type Board = HexBoard Field

isValidMove :: Move -> Board -> Bool
isValidMove (MoveInsert p (owner, unit)) board = numberOfFriendFields >= 1 && numberOfEnemyFields == 0
    where
        numberOfFriendFields = numberOfFields owner
        numberOfEnemyFields = numberOfFields $ playerCounterpart owner
        numberOfFields player = length . filter ((== player) . fst) . catMaybes $ neighbours p board    

isValidMove (MoveFromTo from to) board = 
    case lookup from board of
        Just (o, u) -> isValidUnitMove u board (from, to)
        Nothing -> False -- no unit at start point obviously means invalid move

distinct :: [Point] -> [Point]
distinct [] = []
distinct (x:xs) = x : (distinct $ filter (\p -> p /= x) xs)


allCordsFromPiece :: Point -> Board -> [Point]
allCordsFromPiece p b  
	| length pieceNeighbors == 0 = [p]
	| otherwise = p : (
		foldl (\acc x -> acc ++ (allCordsFromPiece x $ delete p b)) [] pieceNeighbors
	)
	where
		pieceNeighbors = existingNeighboursCords p b

isDestroyingHive :: Coord c => c -> Board -> Bool
isDestroyingHive from board = length (distinct connected) /= (length $ toList board)
	where
		randomPiece = existingNeighboursCords from board !! 0
		connected = allCordsFromPiece randomPiece $ delete from board


-- The owner of the unit isn't necessary to find out if the move is valid.
isValidUnitMove :: Coord c => Unit -> Board -> (c, c) -> Bool
isValidUnitMove Ant b (from, to) = undefined
isValidUnitMove Spider b (from, to) = undefined
isValidUnitMove _ _ _ = undefined

type Direction = Int
{-
directions

    0 
 5     1
 4     2
    3
-}

type MoveSet = Set.Set AxialCoord

-- 
expandSingleMove :: Coord c => Board -> c -> MoveSet
expandSingleMove b c = Set.fromList . map (moved c) . filter isValidDirection $ [0..5]
    where
        n = neighbours c b
        getN d = n !! (d `mod` 6)

        isValidDirection :: Int -> Bool
        isValidDirection d = (isNothing $ getN d) && ((isJust $ getN (d-1)) `xor` (isJust $ getN (d+1)))
            where
                xor :: Bool -> Bool -> Bool
                xor True = not
                xor False = id

        moved :: Coord c => c -> Direction -> AxialCoord
        moved c d = (AxialCoord $ neighbourOffsets !! d) <+> (toAxial c)

-- hive graph traversal
expandAntMoves :: Board -> MoveSet -> MoveSet
expandAntMoves b m =
    let
        newMoves :: MoveSet
        newMoves = foldl1 Set.union . map (expandSingleMove b) . Set.toList $ m

        diff = Set.difference newMoves m
    in
        if Set.size diff > 0 then
            expandAntMoves b (m `Set.union` newMoves)
        else
            m
        

-- remove the original ant from the spot
validAntMoves :: Coord c => c -> Board -> [AxialCoord]
validAntMoves c b = Set.toList $ expandAntMoves (delete c b) (Set.fromList [toAxial c])

validUnitMoves :: Coord c => c -> Board -> [AxialCoord]
validUnitMoves c b = 
	if isDestroyingHive c b then
		[]
	else
		case lookup c b of
			Just (_, u) ->			
				case u of
					Ant -> validAntMoves c b
					_ -> []
			Nothing -> []

playerCounterpart :: Player -> Player
playerCounterpart PlayerBlack = PlayerWhite
playerCounterpart PlayerWhite = PlayerBlack

toString :: Board -> String
toString board = unlines . chunksOf 10 . map fieldToChar $ range (AxialCoord (0, 0), AxialCoord (9, 9))
    where 
        fieldToChar :: Point -> Char
        fieldToChar point = case lookup point board of
            Just (_, unit) -> unitToChar unit
            Nothing -> '.'
 
unitToChar :: Unit -> Char
unitToChar (Beetle _) = 'B'
unitToChar Ant = 'A'
unitToChar Spider = 'S'
unitToChar Grasshopper = 'G'
unitToChar Queen = 'Q'
