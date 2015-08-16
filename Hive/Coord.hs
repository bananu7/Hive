module Hive.Coord where

import Data.Ix

newtype OffsetCoord = OffsetCoord (Int, Int) deriving (Eq, Show, Ord, Ix)
newtype AxialCoord = AxialCoord (Int, Int) deriving (Eq, Show, Ord, Ix)
newtype CubeCoord = CubeCoord (Int, Int, Int) deriving (Eq, Show, Ord)

(<+>) :: AxialCoord -> AxialCoord -> AxialCoord
(AxialCoord (x,y)) <+> (AxialCoord (a,b)) = AxialCoord (x+a, y+b)

toPixel :: Coord c => Int -> c -> (Int, Int)
toPixel sz c = offsetToPixel sz $ toOffset c

offsetToPixel :: Int -> OffsetCoord -> (Int, Int)
offsetToPixel sz (OffsetCoord (ix,iy)) = 
    if (abs ix) `mod` 2 == 0
        then (floor $ x * dw, floor $ y * dh)
        else (floor $ x * dw, floor $ (y + 0.5) * dh)
    where
        x = fromIntegral ix
        y = fromIntegral iy
        w = fromIntegral sz
        h = (sqrt(3) * 0.5 * w)
        dw = 0.75 * w
        dh = h


class Coord c where
    toAxial :: c -> AxialCoord
    toOffset :: c -> OffsetCoord
    toCube :: c -> CubeCoord

instance Coord OffsetCoord where
    toOffset = id
    toCube (OffsetCoord (col, row)) =
        -- odd-Q
        let
            x = col
            z = row - (col - ((abs col) `mod` 2)) `div` 2
            y = -x-z
        in
            CubeCoord (x,y,z)
        -- even-Q
        {-
        let
            x = col
            z = row - (col + ((abs col) `mod` 2)) `div` 2
            y = -x-z
        in
            CubeCoord (x, y, z)
        -}
    toAxial = toAxial . toCube

instance Coord AxialCoord where
    toAxial = id
    toCube (AxialCoord (q, r)) =
        let
            x = q
            z = r
            y = -x-z
        in
            CubeCoord (x, y, z)

    toOffset = toOffset . toCube

instance Coord CubeCoord where
    toCube = id
    toAxial (CubeCoord (x, _, z)) = AxialCoord (x, z)

    toOffset (CubeCoord (x, _, z)) =
        -- odd-Q        
        OffsetCoord (x, z + (x - ((abs x) `mod` 2)) `div` 2)
        -- even-Q
        --OffsetCoord (x, z + (x + ((abs x) `mod` 2)) `div` 2)
