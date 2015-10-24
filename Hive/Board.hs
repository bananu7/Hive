module Hive.Board where

import Prelude hiding (lookup)
import Hive.Coord
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)

data HexBoard a = HexBoard (Map.Map AxialCoord a)

emptyBoard :: HexBoard a
emptyBoard = HexBoard $ Map.fromList []

lookup :: Coord c => c -> HexBoard a -> Maybe a
lookup c (HexBoard b) = Map.lookup (toAxial c) b

toList :: HexBoard a -> [(AxialCoord, a)]
toList (HexBoard b) = Map.toList b

insert :: Coord c => c -> a -> HexBoard a -> HexBoard a
insert c a (HexBoard b) = HexBoard $ Map.insert (toAxial c) a b

delete :: Coord c => c -> HexBoard a -> HexBoard a
delete c (HexBoard b) = HexBoard $ Map.delete (toAxial c) b

neighbourOffsets :: [(Int, Int)]
neighbourOffsets = [
    (0, negate 1),
    (1, negate 1),
    (1, 0),
    (0, 1),
    (negate 1, 1),
    (negate 1, 0)
    ]

-- in "clock" order, starting at 12.
neighbourCoords :: (Coord c) => c -> [AxialCoord]
neighbourCoords c = axialNeighbours . toAxial $ c
    where
        axialNeighbours c@(AxialCoord (q, r)) = map ((<+> c) . AxialCoord) $ neighbourOffsets

neighbours :: Coord c => c -> HexBoard a -> [Maybe a]
neighbours c b = map ((flip lookup) b) . neighbourCoords $ c

existingNeighboursCords :: Coord c => c -> HexBoard a -> [AxialCoord]
existingNeighboursCords c b = filter (\cord -> exists cord) cords
	where
		cords =  neighbourCoords c
		exists c = isJust $ lookup c b

