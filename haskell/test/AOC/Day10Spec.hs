module AOC.Day10Spec (spec) where

import AOC.Day10
import Test.Hspec

testMap :: TileMap
testMap = unsafeParseMap
    [ ".....",--0
      "...F.",--1
      ".---.",--2
      ".....",--3
      "S-|-." --4
    ] 
    -- 01234


spec :: Spec
spec = do
  describe "Day10" $ do
    describe "getConnected" $ do
      it "deals with map boundaries correctly" $ do
        getConnections (0,0) testMap `shouldBe` None
        getConnections (4,4) testMap `shouldBe` None
      it "yields no connections if there is only Ground" $ do
        getConnections (1,1) testMap `shouldBe` None
      it "finds east-west 2-way connection" $ do
        getConnections (2,2) testMap `shouldBe` TwoWay (3,2) (1,2)
      it "finds no connections if tiles don't fit" $ do
        getConnections (2,4) testMap `shouldBe` None
      it "finds connections for S" $ do
        getConnections (0,4) testMap `shouldBe` OneWay (1,4)

