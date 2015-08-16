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
    _sprites :: [(Unit, Sprite)],
    _board :: Board
}
makeLenses ''SampleState

imageData :: [(Unit, String)]
imageData = [
    (Queen, "queen.png"),
    (Spider, "spider.png"),
    (Grasshopper, "hopper.png"),
    (Beetle Nothing, "beetle.png"),
    (Ant, "ant.png")
    ]

loadSprites :: IO [(Unit, Sprite)]
loadSprites = do
    let withPaths = map (Control.Lens._2 %~ ("img/" ++)) imageData
    mapM turnIntoSprite withPaths
    where
        turnIntoSprite :: (Unit, String) -> IO (Unit, Sprite)
        turnIntoSprite (a, b) = do
            b' <- loadSprite b
            return (a,b')

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$> loadSprites
                         <*> pure startBoard

startBoard = emptyBoard
    & insert (OffsetCoord (1, 0)) (PlayerBlack, Spider)
    & insert (OffsetCoord (2, 0)) (PlayerBlack, Queen)
    & insert (OffsetCoord (3, 0)) (PlayerBlack, Ant)
    & insert (OffsetCoord (1, 1)) (PlayerBlack, Ant)
    & insert (OffsetCoord (1, 2)) (PlayerBlack, Ant)
    

coordToPixel pos = Vec2 (fromIntegral x) (fromIntegral y)
    where (x,y) = toPixel 200 pos

sampleDraw :: DrawFn SampleState
sampleDraw s = map drawPiece $ toList (s ^. board)
    where
        drawPiece (pos, (_, unit)) = translate (coordToPixel pos) $ sprite TopLeft (findSprite unit)

        findSprite :: Unit -> Sprite
        findSprite u = fromJust $ List.lookup u (s ^. sprites)


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
