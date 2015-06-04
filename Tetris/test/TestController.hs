module Main (main) where

import Test.Hspec

import Controller

main :: IO ()
main = hspec $ describe "Testing the control operations" $ do
  describe "test addition" $ do
    it "should have an identity" $ do
      (0 + 1,1 + 0) `shouldBe` (1, 1)
