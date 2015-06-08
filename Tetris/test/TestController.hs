module Main (main) where

import Control.Lens
import Data.Array
import Test.Hspec

import Controller
import Tetris
import Utils

main :: IO ()
main = hspec $ describe "Testing the control operations" $ do
  describe "test extractLocs on all Tetriminos" $ do
    it "should work" $ do
      let tet1 = I [(1,2)] (4,5)
      let tet2 = J [(1,5),(3,4)] (43,2)
      let tet3 = L [(5,6),(7,8),(9,10)] (12,3)
      let tet4 = O [(0,1),(2,3)] (2,43)
      let tet5 = S [(9,1000)] (5,7)
      let tet6 = T [(0,12)] (3,4)
      let tet7 = Z [(9,1000), (100,7)] (3,2)
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
      let tet = [((1,y), Filled (I locs (2,3))) | y <- [1..4]]
      let midd = [((2,y), Empty) | y <- [1..4]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tnyBd = array ((0,0),(3,4)) (lft ++ rgt ++ btm ++ tet ++ midd)
      isBarrier (1,1-1) locs tnyBd `shouldBe` True
    it "should not be a barrier if its empty" $ do
      let locs = [(1,2),(1,3),(1,4),(1,5)]
      let lft = [((0,y), Wall) | y <- [0..5]]
      let rgt = [((3,y), Wall) | y <- [0..5]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tet = [((1,y), Filled (I locs (4,5))) | y <- [2..5]]
      let midd = ((1,1), Empty):[((2,y), Empty) | y <- [1..5]]
      let tnyBd = array ((0,0),(3,5)) (lft ++ rgt ++ btm ++ tet ++ midd)
      isBarrier (1,2-1) locs tnyBd `shouldBe` False
  describe "Check hasLanded" $ do
    it "should be true when it lands on the floor" $ do
      let locs = [(1,1),(1,2),(1,3),(1,4)]
      let lft = [((0,y), Wall) | y <- [0..4]]
      let rgt = [((3,y), Wall) | y <- [0..4]]
      let tet = [((1,y), Filled (I locs (4,5))) | y <- [1..4]]
      let midd = [((2,y), Empty) | y <- [1..4]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tnyBd = array ((0,0),(3,4)) (lft ++ rgt ++ btm ++ tet ++ midd)
      hasLanded locs tnyBd `shouldBe` True
    it "Shold be false when its a block above floor" $ do
      let locs = [(1,5),(1,2),(1,3),(1,4)]
      let lft = [((0,y), Wall) | y <- [0..5]]
      let rgt = [((3,y), Wall) | y <- [0..5]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tet = [((1,y), Filled (I locs (4,5))) | y <- [2..5]]
      let midd = ((1,1),Empty):[((2,y), Empty) | y <- [1..5]]
      let tnyBd = array ((0,0),(3,5)) (lft ++ rgt ++ btm ++ tet ++ midd)
      hasLanded locs tnyBd `shouldBe` False
    it "Should be true when it lands on another one" $ do
      let sqr = O [(1,1),(2,2),(1,2),(2,1)] (4,5)
      let locs = [(1,3),(1,4),(1,5),(1,6)]
      let lft = [((0,y), Wall) | y <- [0..6]]
      let rgt = [((3,y), Wall) | y <- [0..6]]
      let tet = [((1,y), Filled (I locs (4,5))) | y <- [3..6]]
      let friend = [((i,j), Filled sqr) | i <- [1..2], j <- [1..2]]
      let midd = [((2,y), Empty) | y <- [3..6]]
      let btm = [((x,0), Wall) | x <- [1..2]]
      let tnyBd = array ((0,0),(3,6)) (lft ++ rgt ++ btm ++ tet ++ midd ++ friend)
      hasLanded locs tnyBd `shouldBe` True
    it "Can land in the sense that it is hanging" $ do
      let line = I [(1,1),(1,2),(1,3),(1,4)] (4,5)
      let me = [(1,5),(2,5),(2,4),(2,3)]
      let lft = [((0,y), Wall) | y <- [0..5]]
      let rgt = [((3,y), Wall) | y <- [0..5]]
      let tet = [(xy, Filled (J me (4,5))) | xy <- me]
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
      ps == ps' `shouldBe` False
      ps == (map (\(x,y) -> (x,y+1)) ps') `shouldBe` True
    it "can move right" $ do
      let stR = moveFalling st Rgt
      let tR = view falling stR
      let psR = extractLocs tR
      ps == psR `shouldBe` False
      ps == (map (\(x,y) -> (x-1,y)) psR) `shouldBe` True
    it "can move left" $ do
      let stL = moveFalling st Lft
      let tL = view falling stL
      let psL = extractLocs tL
      ps == psL `shouldBe` False
      ps == (map (\(x,y) -> (x+1,y)) psL) `shouldBe` True
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
      let ps3 = [(2,3),(2,2),(2,1),(2,0)]
      let ps4 = [(1,1),(2,1),(3,1),(0,1)]
      rotate (I ps1 (2,2)) `shouldBe` (I ps2 (2,2))
      rotate (I ps2 (2,2)) `shouldBe` (I ps3 (2,2))
      rotate (I ps3 (2,2)) `shouldBe` (I ps4 (2,2))
      rotate (I ps4 (2,2)) `shouldBe` (I ps1 (2,2))
    it "should properly rotate S" $ do
      let ps1 = [(0,1),(1,1),(1,2),(2,2)]
      let ps2 = [(1,2),(1,1),(2,1),(2,0)]
      let ps3 = [(0,0),(1,0),(1,1),(2,1)]
      let ps4 = [(0,2),(0,1),(1,1),(1,0)]
      rotate (S ps1 (1.5,1.5)) `shouldBe` (S ps2 (1.5,1.5))
      rotate (S ps2 (1.5,1.5)) `shouldBe` (S ps3 (1.5,1.5))
      rotate (S ps3 (1.5,1.5)) `shouldBe` (S ps4 (1.5,1.5))
      rotate (S ps4 (1.5,1.5)) `shouldBe` (S ps1 (1.5,1.5))
    it "should update board" $ do
      let ps = [(2,2),(2,3),(2,4),(2,5)]
      let emp = [((x,y), Empty) | x<-[1..5], y<-[1..5], not $ (x,y) `elem` ps]
      let l = [((0,y), Wall) | y<-[0..5]]
      let r = [((6,y), Wall) | y<-[0..5]]
      let b = [((x,0), Wall) | x<-[1..5]]
      let t = [(xy, Filled (I ps (3,4))) | xy <- ps]
      let bd = array ((0,0),(6,5)) (t ++ b ++ r ++ l ++ emp)
      let st = State bd (I ps (3,4)) 1 0.4 0 1 Active (drop 2 initBag) [] "" 0 []
      let st' = doRotation st
      let bd' = view board st'
      let f' = view falling st'
      let ps' = extractLocs f'
      view landedTets st' `shouldBe` []
      rotate (I ps (3,4)) `shouldBe` f'
      ps' `shouldBe` [(1,4),(2,4),(3,4),(4,4)]
      all (\xy -> bd' ! xy == (Filled (I ps' (3,4)))) ps' `shouldBe` True
      all (\xy -> bd' ! xy == Empty) [(2,2),(2,3),(2,5)] `shouldBe` True
  describe "test the game over" $ do
    let f = S [(2,20),(2,19),(3,19),(3,18)] (2,3)
    let line = I [(1,17),(1,18),(1,19),(1,20)] (23,3)
    let l = [((0,y), Wall) | y <- [0..20]]
    let r = [((12,y), Wall) | y <- [0..20]]
    let b = [((x,0), Wall) | x <- [1..10]]
    let mid = [((x,y), Empty) | x<-[1..10]
                              , y<-[1..20]
                              , not $ (x,y) `elem` (extractLocs line)
                              , not $ (x,y) `elem` (extractLocs f)]
    let fp = [(xy, Filled f) | xy <- (extractLocs f)]
    let other = [(xy, Filled line) | xy <- (extractLocs line)]
    let bd = array ((0,0),(12,20)) (l ++ r ++ b++ mid ++fp ++ other)
    let st = State bd f 1 0 1 Active (initBag) [] 0 []
    let st' = gameOver st
    let bd' = view board st'
    it "Should detect when there is a piece that causes the game to be over" $ do
      bd' == initBoard `shouldBe` True
    it "Should change state" $ do
      view gameSt st' `shouldBe` Over
  describe "test clearing rows" $ do
    let i1 = I [(1,1),(2,1),(3,1),(4,1)] (2,3)
    let i2 = I [(5,1),(6,1),(7,1),(8,1)] (5,1)
    let i3 = I [(1,2),(1,3),(1,4),(1,5)] (2,10)
    let i4 = I [(2,2),(3,2),(4,2),(5,2)] (3,5)
    let i5 = I [(6,2),(7,2),(8,2),(9,2)] (9,9)
    let j = J [(9,1),(10,1),(10,2),(10,3)] (1,10)
    let f = I [(1,17),(1,18),(1,19),(1,20)] (23,3)
    let l = [((0,y), Wall) | y <- [0..20]]
    let r = [((12,y), Wall) | y <- [0..20]]
    let b = [((x,0), Wall) | x <- [1..10]]
    let mid = [((x,y), Empty) | x<-[1..10]
                              , y<-[1..20]
                              , not $ (x,y) `elem` (extractLocs f)
                              , not $ (x,y) `elem` (extractLocs j)
                              , not $ (x,y) `elem` (extractLocs i1)
                              , not $ (x,y) `elem` (extractLocs i2)
                              , not $ (x,y) `elem` (extractLocs i3)
                              , not $ (x,y) `elem` (extractLocs i4)
                              , not $ (x,y) `elem` (extractLocs i5)
                              ]
    let jP = [(xy, Filled j) | xy <- (extractLocs j)]
    let iP1 = [(xy, Filled i1) | xy <- (extractLocs i1)]
    let iP2 = [(xy, Filled i2) | xy <- (extractLocs i2)]
    let iP3 = [(xy, Filled i3) | xy <- (extractLocs i3)]
    let iP4 = [(xy, Filled i4) | xy <- (extractLocs i4)]
    let iP5 = [(xy, Filled i5) | xy <- (extractLocs i5)]
    let other = [(xy, Filled f) | xy <- (extractLocs f)]
    let bd = array ((0,0),(12,20))
        (l ++ r ++ b++ mid ++ jP ++ other ++ iP1 ++iP2++iP3++iP4++iP5 )
    let st = State bd f 1 0 1 Active (initBag) [] 0 []
    let st' = clearRows st
    it "should detect 2 full rows" $ do
      countRuns bd (extractLocs f) `shouldBe` 2
    it "should detect that rows 1 and 2 are full" $ do
      rowIsFull 1 bd (extractLocs f) `shouldBe` True
      rowIsFull 2 bd (extractLocs f) `shouldBe` True
    it "should not call row 3 full" $ do
      rowIsFull 3 bd (extractLocs f) `shouldBe` False
    it "should properly clear those rows" $ do
      let bd' = view board st
      all (\x -> Empty == (bd' ! (x,1))) [2..9] `shouldBe` True
      all (\x -> Empty == (bd' ! (x,2))) [2..10] `shouldBe` True
      all (\x -> Empty == (bd' ! (x,3))) [2..10] `shouldBe` True
      all (\x -> Empty == (bd' ! (x,4))) [1..10] `shouldBe` True
    it "should increase the score and high score" $ do
      (view score st') > 0 `shouldBe` True
      (view highScore st') == (view score st') `shouldBe` True
