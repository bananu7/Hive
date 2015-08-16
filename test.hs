import Hive
import Hive.Board

import Test.Hspec
import qualified Data.Map.Strict as Map

c :: (Int, Int) -> AxialCoord
c = toAxial . OffsetCoord

{--ODD-Q

             /2,0\
             \___/ /3,0\
             /2,1\ \___/
             \___/ /3,1\
             /2,2 \\___/
             \___ /3,2\
             /2,3 \___/
             \___//3,3\
             /2,4\\WQ_/
             \___ /3,4\
                  \BQ_/
--}

oneUnitMap = insert (c (3, 3)) (PlayerWhite, Queen) emptyBoard
twoUnitMap = insert (c (3, 4)) (PlayerBlack, Queen) oneUnitMap

players = [PlayerWhite, PlayerBlack]

main :: IO ()
main = hspec $ do
    describe "Hive's move logic" $ do
        it "prevents placing the unit on top of another unit" $ do
            isValidMove (MoveInsert (c (3, 3)) (PlayerBlack, Spider)) oneUnitMap `shouldBe` False
            isValidMove (MoveInsert (c (3, 3)) (PlayerBlack, Beetle Nothing)) oneUnitMap `shouldBe` False
        it "requires a neighbouring ally" $ do
            isValidMove (MoveInsert (c (3, 2)) (PlayerWhite, Spider)) oneUnitMap `shouldBe` True
            isValidMove (MoveInsert (c (3, 1)) (PlayerWhite, Spider)) oneUnitMap `shouldBe` False
        it "requires no neighbouring enemy" $ do
            isValidMove (MoveInsert (c (2, 3)) (PlayerWhite, Spider)) twoUnitMap `shouldBe` True
            isValidMove (MoveInsert (c (2, 4)) (PlayerWhite, Spider)) twoUnitMap `shouldBe` False
