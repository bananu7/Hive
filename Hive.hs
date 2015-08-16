module Hive
    ( module Hive.Board
    , module Hive.Coord
    , Board
    , Unit(..)
    , Player(..)
    , Move(..)
    , isValidMove
    , Point
    )
where

import Prelude hiding (lookup)
import Data.List.Split (chunksOf)
import Data.Ix (range)
import Data.Maybe (catMaybes)

import Hive.Board
import Hive.Coord

data Unit = Beetle (Maybe Unit) | Ant | Spider | Grasshopper | Queen deriving (Eq, Show)

data Player = PlayerWhite | PlayerBlack deriving (Eq, Show)

type Point = AxialCoord
type Field = (Player, Unit)
data Move = MoveFromTo Point Point | MoveInsert Point Field

type Board = HexBoard Field

isValidMove :: Move -> Board -> Bool
isValidMove (MoveInsert p (owner, unit)) board = isNextToFriendButNotEnemy ||
                                                 isFirstMoveInGame ||
                                                 isValidSecondMoveInGame
    where
        isNextToFriendButNotEnemy = numberOfFriendFields >= 1 && numberOfEnemyFields == 0
        isFirstMoveInGame = numberOfFieldsOnBoard == 0
        isValidSecondMoveInGame = numberOfFieldsOnBoard == 1 && numberOfFriendFields == 0 && isNextToEnemy
        isNextToEnemy = numberOfEnemyFields >= 1
        numberOfFriendFields = numberOfFields owner
        numberOfEnemyFields = numberOfFields $ playerCounterpart owner
        numberOfFields player = length . filter ((== player) . fst) $ neighbours p board
        numberOfFieldsOnBoard = size board 

isValidMove (MoveFromTo _ _) board = undefined

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
