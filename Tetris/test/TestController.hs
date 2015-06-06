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
  describe "Test moveFalling" $ do
    let st = initState
    let t = view falling st
    let ps = extractLocs t
    let st' = moveFalling st Down
    let t' = view falling st'
    let ps' = extractLocs t'
    it "Has 4 locations before and after" $ do
      length ps `shouldBe` 4
      length ps' `shouldBe` 4
    it "Is one farther down in y" $ do
      --putStrLn $ show ps'
      ps == ps' `shouldBe` False
      ps == (map (\(x,y) -> (x,y+1)) ps') `shouldBe` True
     -- `shouldBe` True
    it "can move right" $ do
      let stR = moveFalling st Rgt
      let tR = view falling stR
      let psR = extractLocs tR
      ps == psR `shouldBe` False
      ps == (map (\(x,y) -> (x+1,y)) psR) `shouldBe` True
  describe "Test advanceFalling" $ do
    let st = initState
    let t = view falling st
    let st' = advanceFalling st Down
    let t' = view falling st'
    let ps' = extractLocs t'
    it "The new falling piece should have 4 locations" $ do
      length (extractLocs t) `shouldBe` 4
      length ps' `shouldBe` 4
  describe "Test the board is modified" $ do
    let st = initState
    let t = view falling st
    let bd = view board st
    let st' = advanceFalling st Down
    let bd' = view board st'
    let t' = view falling st'
    it "changes the board" $ do
      bd == bd' `shouldBe` False
      (extractLocs t) == (extractLocs t') `shouldBe` False
    it "adds the tet's new positions to the board" $ do
      let ps = extractLocs t'
      all (\xy -> (bd' ! xy) == (Filled t')) ps `shouldBe` True
      all (\xy -> (bd ! xy) == (Filled t')) ps `shouldBe` False
  describe "test rotation" $ do
    it "should properly rotate I" $ do
      let ps1 = [(1,0),(1,1),(1,2),(1,3)]
      let ps2 = [(0,2),(1,2),(2,2),(3,2)]
      let ps3 = [(2,3),(2,2), (2,1),(2,0)]
      let ps4 = [(1,1),(2,1), (3,1),(0,1)]
      findCenter ps1 `shouldBe` 2
      rotate (I ps1) `shouldBe` (I ps2)
      rotate (I ps2) `shouldBe` (I ps3)
      rotate (I ps3) `shouldBe` (I ps4)
      rotate (I ps4) `shouldBe` (I ps1)
    it "should properly rotate S" $ do
      let ps1 = [(0,1), (1,1), (1,2), (2,2)]
      let ps2 = [(1,2),(1,1),(2,1),(2,0)]
      let ps3 = [(0,0),(1,0),(1,1),(2,1)]
      let ps4 = [(0,2),(0,1),(1,1),(1,0)]
      findCenter ps1 `shouldBe` 1.5
      rotate (S ps1) `shouldBe` (S ps2)
      rotate (S ps2) `shouldBe` (S ps3)
      rotate (S ps3) `shouldBe` (S ps4)
      rotate (S ps4) `shouldBe` (S ps1)
    it "should update board" $ do
      let ps = [(2,2),(2,3),(2,4),(2,5)]
      let emp = [((x,y), Empty) | x<-[1..5], y<-[1..5], not $ (x,y) `elem` ps]
      let l = [((0,y), Wall) | y<-[0..5]]
      let r = [((6,y), Wall) | y<-[0..5]]
      let b = [((x,0), Wall) | x<-[1..5]]
      let t = [(xy, Filled (I ps)) | xy <- ps]
      let bd = array ((0,0),(6,5)) (t ++ b ++ r ++ l ++ emp)
      let st = State bd (I ps) 1 0.4 0 1 Active (drop 2 initBag) [] "" 0 []
      let st' = doRotation st
      let bd' = view board st'
      let f' = view falling st'
      let ps' = extractLocs f'
      view landedTets st' `shouldBe` []
      rotate (I ps) `shouldBe` f'
      ps' `shouldBe` [(1,4),(2,4),(3,4),(4,4)]
      all (\xy -> bd' ! xy == (Filled (I ps'))) ps' `shouldBe` True
      all (\xy -> bd' ! xy == Empty) [(2,2),(2,3),(2,5)] `shouldBe` True
  describe "test the game over" $ do
    let f = S [(2,3),(2,4),(3,3),(3,2)]
    let line = I [(1,1),(1,2),(1,3),(1,4)]
    let l = [((0,y), Wall) | y <- [0..4]]
    let r = [((4,y), Wall) | y <- [0..4]]
    let b = [((x,0), Wall) | x <- [1..3]]
    let mid = [((2,1), Empty), ((2,2), Empty), ((3,1), Empty), ((3,4), Empty)]
    let fp = [(xy, Filled f) | xy <- (extractLocs f)]
    let other = [(xy, Filled line) | xy <- (extractLocs line)]
    let bd = array ((0,0),(4,4)) (l ++ r ++ b++ mid ++fp ++ other)
    let st = State bd f 1 0.4 0 1 Active (initBag) [] "" 0 []
    let st' = gameOver st
    let bd' = view board st'
    it "Should detect when there is a piece that causes the game to be over" $ do
      bd' == initBoard `shouldBe` True
