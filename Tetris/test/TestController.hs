module Main (main) where

import Test.Hspec

import Controller
import Tetris

main :: IO ()
main = hspec $ describe "Testing the control operations" $ do
  describe "test addition" $ do
    it "should have an identity" $ do
      (0 + 1,1 + 0) `shouldBe` (1, 1)
  describe "Check hasLanded" $ do
    it "should be true when it lands on the floor" $ do
      let locs = [(1,1),(1,2),(1,3),(1,4)]
          lft = [((0,y), Wall) | y <- [0..4]]
          rgt = [((3,y), Wall) | y <- [0..4]]
          tet = [((1,y), I locs) | y <- [1..4]]
          midd = [((2,y), Empty) | y <- [1..4]]
          btm = [((x,0), Wall) | x <- [1..2]]
          tnyBd = array ((0,0),(3,4)) (lft ++ rgt ++ btm ++ tet ++ midd)
        in
       hasLanded locs tnyBd `shouldBe` True
    it "Shold be false when its a block above floor" $ do
      let locs = [(1,1),(1,2),(1,3),(1,4)]
          lft = [((0,y), Wall) | y <- [0..5]]
          rgt = [((3,y), Wall) | y <- [0..5]]
          btm = [((x,0), Wall) | x <- [1..2]]
          tet = [((1,y), I locs) | y <- [2..5]]
          midd = [((2,y), Empty) | y <- [1..5]]
          tnyBd = array ((0,0),(3,5)) (lft ++ rgt ++ btm ++ tet ++ midd)
        in
       hasLanded locs tnyBd `shouldBe` False
    it "Should be true when it lands on another one" $ do
      let sqr = O [(1,1),(2,2),(1,2),(2,1)]
          me = [(1,3),(1,4),(1,5),(1,6)]
          lft = [((0,y), Wall) | y <- [0..6]]
          rgt = [((3,y), Wall) | y <- [0..6]]
          tet = [((1,y), I locs) | y <- [3..6]]
          friend = [((i,j), sqr) | i <- [1..2], j <- [1..2]]
          midd = [((2,y), Empty) | y <- [3..6]]
          btm = [((x,0), Wall) | x <- [1..2]]
          tnyBd = array ((0,0),(3,6)) (lft ++ rgt ++ btm ++ tet ++ midd ++ friend)
        in
       hasLanded locs tnyBd `shouldBe` True
    it "Can land in the sense that it is hanging" $ do
      let line = I [(1,1),(1,2),(1,3),(1,4)]
          me = [(1,5),(2,5),(2,4),(2,3)]
          lft = [((0,y), Wall) | y <- [0..5]]
          rgt = [((3,y), Wall) | y <- [0..5]]
          tet = [(xy, J locs) | xy <- locs]
          friend [((1,y), line) | y <- [1..4]]
          midd = [((2,y), Empty) | y <- [1..2]]
          btm = [((x,0), Wall) | x <- [1..2]]
          tnyBd = array ((0,0),(3,5)) (lft ++ rgt ++ btm ++ tet ++ midd ++ friend)
        in
       hasLanded me `shouldBe` True
