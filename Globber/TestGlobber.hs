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
      it "matches any character within a longer string" $
        matchGlob "?s" "zs" `shouldBe` True
      it "matches characters within a longer string 2" $
        matchGlob "x?s" "xzs" `shouldBe` True
      it "doesn't match the empty string" $
        matchGlob "?" " " `shouldBe` False
    describe "case: star" $ do
      it "matches any string" $
        matchGlob "*" "xyz" `shouldBe` True
      it "matches any string at end of longer string" $
        matchGlob "ab*" "abXYZ" `shouldBe` True
      it "matches any string within a longer string" $
        matchGlob "ab*bc" "abXYZbc" `shouldBe` True
      it "doesn't match when more patterns follow that dont match" $
        matchGlob "ab*bc" "abXYZmn" `shouldBe` False
      it "does match when more patterns follow that match" $
        matchGlob "ab*bc" "abXYZASEWERbc" `shouldBe` True
      it "does match with use of question mark case" $
        matchGlob "ab*?d" "abXYZmd" `shouldBe` True
      it "matches with multiple stars" $
        matchGlob "***************************************" "aaaaaaaa" `shouldBe` True
    describe "case: escape character" $ do
      it "matches a single escaped character" $
        matchGlob "\\a" "a" `shouldBe` True
      it "matches a single escaped special character" $
        matchGlob "\\*" "*" `shouldBe` True
      it "matches a single escaped character at the end of a string" $
        matchGlob "ab*\\*" "abXYZ*" `shouldBe` True
      it "matches an escaped character within a string" $
        matchGlob "ab?\\?mno" "abZ?mno" `shouldBe` True
      it "doesn't match wrong escaped character" $
        matchGlob "\\a\\b\\c\\e" "abcd" `shouldBe` False

    -- describe "case: edge cases"
