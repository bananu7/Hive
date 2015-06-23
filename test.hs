import Hive

import Test.Hspec
import Test.QuickCheck

oneUnitMap = Map.insert (3,3) (PlayerWhite, Queen) emptyBoard
    where emptyBoard = Map.fromList [] :: Board

players = [PlayerWhite, PlayerBlack]

main :: IO ()
main = hspec $ do
    describe "Hive's move logic" $ do
        it "prevents placing the unit on top of another unit" $ do
            isValidMove (MoveInsert (3,3) (PlayerBlack, Spider)) oneUnitMap `shouldBe` False
            isValidMove (MoveInsert (3,3) (PlayerBlack, Beetle)) oneUnitMap `shouldBe` False

main = do
    let b = Map.fromList [] :: Board
    let c = Map.insert (3,3) (PlayerWhite, Queen) b

    test $ isValidMove (MoveInsert (3,2) (PlayerWhite, Spider)) c
    test $ not $ isValidMove (MoveInsert (3,2) (PlayerBlack, Spider)) c
    test $ not $ isValidMove (MoveInsert (3,3) (PlayerWhite, Spider)) c
    test $ not $ isValidMove (MoveInsert (3,3) (PlayerBlack, Spider)) c
    test $ not $ isValidMove (MoveInsert (3,2) (PlayerWhite, Queen)) c

    where
        test True = print "OK"
        test False = print "FAIL"