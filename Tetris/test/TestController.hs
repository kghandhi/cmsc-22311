module Main (main) where

import Data.Array
import Test.Hspec


import Controller
import Tetris

main :: IO ()
main = hspec $ describe "Testing the control operations" $ do
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
