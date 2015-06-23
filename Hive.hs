module Hive where
 
import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf)
import Data.Ix (range)
import Data.Maybe (catMaybes)

data Unit = Beetle (Maybe Unit) | Ant | Spider | Grasshopper | Queen deriving (Eq, Show)
 
data Player = PlayerWhite | PlayerBlack deriving (Eq, Show)
type Point = (Int, Int)
type Field = (Player, Unit)
type Board = Map.Map Point Field
 
data Move = MoveFromTo Point Point | MoveInsert Point Field
 
(<+>) :: Point -> Point -> Point
(x,y) <+> (a,b) = (x+a, y+b)

isValidMove :: Move -> Board -> Bool
isValidMove (MoveInsert p (owner, unit)) board = numberOfFriendFields >= 1 && numberOfEnemyFields == 0
    where
        numberOfFriendFields = numberOfFields owner
        numberOfEnemyFields = numberOfFields $ playerCounterpart owner
        numberOfFields player = length . filter ((== player) . fst) $ neighboringFields
        neighboringFields = catMaybes . map (((flip Map.lookup) board) . (<+> p)) $ neighbourCoords
        neighbourCoords = [(-1, 0), (1, 0), (-1, -1), (0, -1), (0, 1), (-1, 1)]

isValidMove (MoveFromTo _ _) board = undefined
 
playerCounterpart :: Player -> Player
playerCounterpart PlayerBlack = PlayerWhite
playerCounterpart PlayerWhite = PlayerBlack

toString :: Board -> String
toString board = unlines . chunksOf 10 . map fieldToChar $ range ((0, 0), (9, 9))
    where 
        fieldToChar :: Point -> Char
        fieldToChar point = case Map.lookup point board of
            Just (_, unit) -> unitToChar unit
            Nothing -> '.'
 
unitToChar :: Unit -> Char
unitToChar (Beetle _) = 'B'
unitToChar Ant = 'A'
unitToChar Spider = 'S'
unitToChar Grasshopper = 'G'
unitToChar Queen = 'Q'

