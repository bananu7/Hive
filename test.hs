import Hive

import Test.Hspec
import qualified Data.Map.Strict as Map

oneUnitMap = Map.insert (3,3) (PlayerWhite, Queen) emptyBoard
    where emptyBoard = Map.fromList [] :: Board

twoUnitMap = Map.insert(3,4) (PlayerBlack, Queen) oneUnitMap

players = [PlayerWhite, PlayerBlack]

main :: IO ()
main = hspec $ do
    describe "Hive's move logic" $ do
        it "prevents placing the unit on top of another unit" $ do
            isValidMove (MoveInsert (3,3) (PlayerBlack, Spider)) oneUnitMap `shouldBe` False
            isValidMove (MoveInsert (3,3) (PlayerBlack, Beetle Nothing)) oneUnitMap `shouldBe` False
        it "requires a neighbouring ally" $ do
            isValidMove (MoveInsert (3,2) (PlayerWhite, Spider)) oneUnitMap `shouldBe` True
            isValidMove (MoveInsert (3,1) (PlayerWhite, Spider)) oneUnitMap `shouldBe` False
        it "requires no neighbouring enemy" $ do
            isValidMove (MoveInsert (2,3) (PlayerWhite, Spider)) twoUnitMap `shouldBe` True
            isValidMove (MoveInsert (3,2) (PlayerWhite, Spider)) oneUnitMap `shouldBe` False
