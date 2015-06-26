module Hive.Board where

import Prelude hiding (lookup)
import Hive.Coord
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

data HexBoard a = HexBoard (Map.Map AxialCoord a)

emptyBoard :: HexBoard a
emptyBoard = HexBoard $ Map.fromList []

lookup :: Coord c => c -> HexBoard a -> Maybe a
lookup c (HexBoard b) = Map.lookup (toAxial c) b

toList (HexBoard b) = Map.toList b

insert :: Coord c => c -> a -> HexBoard a -> HexBoard a
insert c a (HexBoard b) = HexBoard $ Map.insert (toAxial c) a b

neighbourCoords :: (Coord c) => c -> [AxialCoord]
neighbourCoords c = axialNeighbours . toAxial $ c
    where
        axialNeighbours c@(AxialCoord (q, r)) = map ((<+> c) . AxialCoord) $ [
            (1, 0),
            (1, negate 1),
            (0, negate 1),
            (negate 1, 0),
            (negate 1, 1),
            (0, 1)
            ]

neighbours :: Coord c => c -> HexBoard a -> [a]
neighbours c b = catMaybes . map ((flip lookup) b) . neighbourCoords $ c
