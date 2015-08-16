{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude hiding (lookup)
import qualified Data.List as List (lookup)

import Hate
import Hate.Graphics
import Vec2Lens (x,y)

import Control.Applicative
import Control.Lens
import Data.Maybe

import Hive
import qualified Data.Map.Strict as Map

-- sample 4

data SampleState = SampleState {
    _sprites :: [((Player, Unit), Sprite)],
    _board :: Board
}
makeLenses ''SampleState

resourcePath = "img/"

imageData :: [((Player, Unit), String)]
imageData = [
    ((PlayerWhite, Queen), "queen.png"),
    ((PlayerWhite, Spider), "spider.png"),
    ((PlayerWhite, Grasshopper), "hopper.png"),
    ((PlayerWhite, Beetle Nothing), "beetle.png"),
    ((PlayerWhite, Ant), "ant_b.png"),
    ((PlayerBlack, Queen), "queen_b.png"),
    ((PlayerBlack, Spider), "spider_b.png"),
    ((PlayerBlack, Grasshopper), "hopper_b.png"),
    ((PlayerBlack, Beetle Nothing), "beetle_b.png"),
    ((PlayerBlack, Ant), "ant_b.png")
    ]

moveSprite = loadSprite $ resourcePath ++ "move.png"

loadSprites :: IO [((Player, Unit), Sprite)]
loadSprites = do
    let withPaths = map (Control.Lens._2 %~ (resourcePath ++)) imageData
    mapM turnIntoSprite withPaths
    where
        turnIntoSprite :: ((Player, Unit), String) -> IO ((Player, Unit), Sprite)
        turnIntoSprite (a, b) = do
            b' <- loadSprite b
            return (a,b')

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$> loadSprites
                         <*> pure startBoard

startBoard = emptyBoard
    & insert (OffsetCoord (3, 3)) (PlayerWhite, Queen)
    & insert (OffsetCoord (3, 4)) (PlayerBlack, Queen)
    & insert (OffsetCoord (3, 2)) (PlayerBlack, Ant)

displayScale :: Float
displayScale = 0.6

coordToPixel :: Coord c => c -> Vec2
coordToPixel pos = Vec2 (fromIntegral x) (fromIntegral y)
    where (x,y) = toPixel (floor $ 200 * displayScale) pos

drawMoves :: Coord c => c -> DrawFn SampleState
drawMoves =
    let 
        b = (s ^. board)
        moves = validUnitMoves c b
        moveToSprite pos = translate (coordToPixel pos) $ sprite TopLeft moveSprite
    in
        map moveToSprite moves

sampleDraw :: DrawFn SampleState
sampleDraw s = map drawPiece $ toList (s ^. board)
    where
        drawPiece (pos, (owner, unit)) = scaled (Vec2 displayScale displayScale) $ translate (coordToPixel pos) $ sprite TopLeft (findSprite (owner, unit))

        findSprite :: (Player, Unit) -> Sprite
        findSprite pu = fromJust $ List.lookup pu (s ^. sprites)


sampleUpdate :: UpdateFn SampleState
sampleUpdate _ = return ()

config :: Config
config = 
    Config
        { windowTitle = "Hive"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
