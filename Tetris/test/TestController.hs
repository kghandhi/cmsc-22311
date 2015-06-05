module Main (main) where

import Control.Lens
import Data.Array
import Test.Hspec

import Controller
import Tetris
import Utils

main :: IO ()
main = hspec $ describe "Testing the control operations" $ do
  describe "test extractLocs, most important fn" $ do
    it "should work" $ do
      let tet1 = I [(1,2)]
      let tet2 = J [(1,5),(3,4)]
      let tet3 = L [(5,6),(7,8),(9,10)]
      let tet4 = O [(0,1),(2,3)]
      let tet5 = S [(9,1000)]
      let tet6 = T [(0,12)]
      let tet7 = Z [(9,1000), (100,7)]
      let tet8 = None
      extractLocs tet1 `shouldBe` [(1,2)]
      extractLocs tet2 `shouldBe` [(1,5),(3,4)]
      extractLocs tet3 `shouldBe` [(5,6),(7,8),(9,10)]
      extractLocs tet4 `shouldBe` [(0,1),(2,3)]
      extractLocs tet5 `shouldBe` [(9,1000)]
      extractLocs tet6 `shouldBe` [(0,12)]
      extractLocs tet7 `shouldBe` [(9,1000),(100,7)]
      extractLocs tet8 `shouldBe` []
  describe "test isBarrier" $ do
    it "should detect walls" $ do
      let locs = [(1,1),(1,2),(1,3),(1,4)]
      let lft = [((0,y), Wall) | y <- [0..4]]
      let rgt = [((3,y), Wall) | y <- [0..4]]
      let tet = [((1,y), Filled (I locs)) | y <- [1..4]]
      let midd = [((2,y), Empty) | y <- [1..4]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tnyBd = array ((0,0),(3,4)) (lft ++ rgt ++ btm ++ tet ++ midd)
      isBarrier (1,1-1) locs tnyBd `shouldBe` True
    it "should not be a barrier if its empty" $ do
      let locs = [(1,2),(1,3),(1,4),(1,5)]
      let lft = [((0,y), Wall) | y <- [0..5]]
      let rgt = [((3,y), Wall) | y <- [0..5]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tet = [((1,y), Filled (I locs)) | y <- [2..5]]
      let midd = ((1,1), Empty):[((2,y), Empty) | y <- [1..5]]
      let tnyBd = array ((0,0),(3,5)) (lft ++ rgt ++ btm ++ tet ++ midd)
      isBarrier (1,2-1) locs tnyBd `shouldBe` False
  describe "Check hasLanded" $ do
    it "should be true when it lands on the floor" $ do
      let locs = [(1,1),(1,2),(1,3),(1,4)]
      let lft = [((0,y), Wall) | y <- [0..4]]
      let rgt = [((3,y), Wall) | y <- [0..4]]
      let tet = [((1,y), Filled (I locs)) | y <- [1..4]]
      let midd = [((2,y), Empty) | y <- [1..4]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tnyBd = array ((0,0),(3,4)) (lft ++ rgt ++ btm ++ tet ++ midd)
      hasLanded locs tnyBd `shouldBe` True
    it "Shold be false when its a block above floor" $ do
      let locs = [(1,5),(1,2),(1,3),(1,4)]
      let lft = [((0,y), Wall) | y <- [0..5]]
      let rgt = [((3,y), Wall) | y <- [0..5]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tet = [((1,y), Filled (I locs)) | y <- [2..5]]
      let midd = ((1,1),Empty):[((2,y), Empty) | y <- [1..5]]
      let tnyBd = array ((0,0),(3,5)) (lft ++ rgt ++ btm ++ tet ++ midd)
      hasLanded locs tnyBd `shouldBe` False
    it "Should be true when it lands on another one" $ do
      let sqr = O [(1,1),(2,2),(1,2),(2,1)]
      let locs = [(1,3),(1,4),(1,5),(1,6)]
      let lft = [((0,y), Wall) | y <- [0..6]]
      let rgt = [((3,y), Wall) | y <- [0..6]]
      let tet = [((1,y), Filled (I locs)) | y <- [3..6]]
      let friend = [((i,j), Filled sqr) | i <- [1..2], j <- [1..2]]
      let midd = [((2,y), Empty) | y <- [3..6]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tnyBd = array ((0,0),(3,6)) (lft ++ rgt ++ btm ++ tet ++ midd ++ friend)
      hasLanded locs tnyBd `shouldBe` True
    it "Can land in the sense that it is hanging" $ do
      let line = I [(1,1),(1,2),(1,3),(1,4)]
      let me = [(1,5),(2,5),(2,4),(2,3)]
      let lft = [((0,y), Wall) | y <- [0..5]]
      let rgt = [((3,y), Wall) | y <- [0..5]]
      let tet = [(xy, Filled (J me)) | xy <- me]
      let friend = [((1,y), Filled line) | y <- [1..4]]
      let midd = [((2,y), Empty) | y <- [1..2]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tnyBd = array ((0,0),(3,5)) (lft ++ rgt ++ btm ++ tet ++ midd ++ friend)
      hasLanded me tnyBd `shouldBe` True
  describe "Test advanceFalling" $ do
    let st = initState
    let t = view falling st
    let st' = advanceFalling st Down
    let t' = view falling st'
    let ps' = extractLocs t'
    it "The new falling piece should have 4 locations" $ do
      length (extractLocs t) `shouldBe` 4
      length ps' `shouldBe` 4
    -- it "Should move the coordinates down 1" $ do
    --   map (
