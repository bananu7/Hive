{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Hate
import Hate.Graphics
import Vec2Lens (x,y)

import Control.Applicative
import Control.Lens
import Data.Maybe

import qualified Hive as H
import qualified Data.Map.Strict as Map

-- sample 4

data SampleState = SampleState {
    _sprites :: [(H.Unit, Sprite)],
    _board :: H.Board
}
makeLenses ''SampleState

imageData :: [(H.Unit, String)]
imageData = [
    (H.Queen, "queen.png"),
    (H.Spider, "spider.png"),
    (H.Grasshopper, "hopper.png"),
    (H.Beetle Nothing, "beetle.png"),
    (H.Ant, "ant.png")
    ]

loadSprites :: IO [(H.Unit, Sprite)]
loadSprites = do
    let withPaths = map (Control.Lens._2 %~ ("img/" ++)) imageData
    mapM turnIntoSprite withPaths
    where
        turnIntoSprite :: (H.Unit, String) -> IO (H.Unit, Sprite)
        turnIntoSprite (a, b) = do
            b' <- loadSprite b
            return (a,b')

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$> loadSprites
                         <*> pure startBoard

startBoard = Map.insert (3,3) (H.PlayerWhite, H.Queen) H.emptyBoard

sampleDraw :: DrawFn SampleState
sampleDraw s = map drawPiece $ Map.toList (s ^. board)
    where
        drawPiece (pos, (_, unit)) = translate (toScreen pos) $ sprite TopLeft (findSprite unit)

        findSprite :: H.Unit -> Sprite
        findSprite u = fromJust $ lookup u (s ^. sprites)

        toScreen :: H.Point -> Vec2
        toScreen (x,y) = Vec2 (fromIntegral x * 200) (fromIntegral y * 200)


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
