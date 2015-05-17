module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "case: empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True
      it "shouldn't match non-empty string" $
        matchGlob "" "string" `shouldBe` False
    describe "case: question mark" $ do
      it "matches any single character" $
        matchGlob "?" "*" `shouldBe` True
