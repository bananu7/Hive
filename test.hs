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
  describe "Hive's place piece logic" $ do
    context "When board is empty" $ do
        it "allows to place piece anywhere on empty board" $
            isValidMove (MoveInsert (c (3, 4)) (PlayerBlack, Spider)) emptyBoard `shouldBe` True
    context "When only one piece is on board" $ do
        it "allows to place piece next to enemy" $
            isValidMove (MoveInsert (c (3, 4)) (PlayerBlack, Spider)) oneUnitMap `shouldBe` True
        it "prevents placing piece not close to enemy" $
            isValidMove (MoveInsert (c (3, 5)) (PlayerBlack, Spider)) oneUnitMap `shouldBe` False
        it "prevents placing enemy unit on top of another unit" $
            isValidMove (MoveInsert (c (3, 3)) (PlayerBlack, Spider)) oneUnitMap `shouldBe` False
        it "prevents placing enemy Beetle on top of another unit" $
            isValidMove (MoveInsert (c (3, 3)) (PlayerBlack, Beetle Nothing)) oneUnitMap `shouldBe` False
        it "prevents placing ally unit on top of another unit" $
            isValidMove (MoveInsert (c (3, 3)) (PlayerWhite, Spider)) oneUnitMap `shouldBe` False
        it "prevents placing ally Beetle on top of another unit" $
            isValidMove (MoveInsert (c (3, 3)) (PlayerWhite, Beetle Nothing)) oneUnitMap `shouldBe` False
    context "When more than one piece is on board" $ do
        it "allows to place piece next to ally" $
            isValidMove (MoveInsert (c (3, 2)) (PlayerWhite, Spider)) twoUnitMap `shouldBe` True
        it "prevents placing piece not close to ally" $
            isValidMove (MoveInsert (c (3, 1)) (PlayerWhite, Spider)) twoUnitMap `shouldBe` False
        it "prevents placing piece next to enemy" $
            isValidMove (MoveInsert (c (2, 4)) (PlayerWhite, Spider)) twoUnitMap `shouldBe` False
