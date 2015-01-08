module Main where

import Test.Hspec
import Utils

main :: IO ()
main = hspec $ do

  describe "decode" $ do
    it "returns null" $ do
      decode [] `shouldBe` []
    it "returns one" $ do
      decode [1] `shouldBe` [" "]
      decode [2] `shouldBe` ["  "]
      decode [3] `shouldBe` ["   "]
    it "returns many" $ do
      decode [1, 1, 1] `shouldBe` [" ", " ", " "]
      decode [1, 2, 3] `shouldBe` [" ", "  ", "   "]

  describe "distribute" $ do
    it "returns null" $ do
      distribute 10 0 `shouldBe` []
    it "returns identity" $ do
      distribute 10 1 `shouldBe` [10]
      distribute 20 1 `shouldBe` [20]
    it "returns interspersed" $ do
      distribute 10 2 `shouldBe`  [5, 5]
      distribute 10 3 `shouldBe`  [3, 3, 4]
      distribute 10 4 `shouldBe`  [2, 2, 3, 3]
      distribute 10 5 `shouldBe`  [2, 2, 2, 2, 2]
      distribute 10 6 `shouldBe`  [1, 1, 2, 2, 2, 2]
      distribute 10 7 `shouldBe`  [1, 1, 1, 1, 2, 2, 2]
      distribute 10 8 `shouldBe`  [1, 1, 1, 1, 1, 1, 2, 2]
      distribute 10 9 `shouldBe`  [1, 1, 1, 1, 1, 1, 1, 1, 2]
      distribute 10 10 `shouldBe` [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

  describe "wordsLength" $ do
    it "returns sum of lengths of words" $ do
      wordsLength []                 `shouldBe` 0
      wordsLength ["a", "b", "c"]    `shouldBe` 3
      wordsLength ["ab", "cd", "ef"] `shouldBe` 6

  describe "intersperseJoin" $ do
    it "intersperses list of strings with another list of strings" $ do
      intersperseJoin ["a", "b", "c"] [":", ";"] `shouldBe` "a:b;c"

  describe "buildLine" $ do
    it "justifies words to certain length" $ do
      buildLine 7 ["a", "b", "c"] `shouldBe` "a  b  c"
      buildLine 8 ["a", "b", "c"] `shouldBe` "a  b   c"
      buildLine 9 ["a", "b", "c"] `shouldBe` "a   b   c"

  describe "linesToWords" $ do
    it "splits lines into list of words" $ do
      linesToWords ["a b", "c"]  `shouldBe` ["a", "b", "c"]
      linesToWords [" a b", "c"] `shouldBe` ["a", "b", "c"]

  describe "breakToLines" $ do
    it "breaks words to paragraph lines" $ do
      breakToLines 10 ["foo", "bar", "baz"] `shouldBe` [["foo", "bar"], ["baz"]]
      breakToLines 15 ["foo", "bar", "baz"] `shouldBe` [["foo", "bar", "baz"]]
      breakToLines 4  ["a", "b", "c", "d"]  `shouldBe` [["a", "b"], ["c", "d"]]

  describe "buildParagraph" $ do
    it "builds paragraph" $ do
      buildParagraph 7 [["a", "b", "c"], ["a", "b", "c"], ["a", "b", "c"]]
        `shouldBe` "a  b  c\na  b  c\na b c\n"

  describe "paragraphs" $ do
    it "splits text onto paragraphs by two or more newlines" $ do
      paragraphs "a\nb\n\nc\nd"                `shouldBe` [["a", "b"], ["c", "d"]]
      paragraphs "\n\na\nb\n\n\nc\nd\n\n\n"    `shouldBe` [["a", "b"], ["c", "d"]]
      paragraphs "\n\na\nb\n\n \n  c\nd\n\n\n" `shouldBe` [["a", "b"], ["c", "d"]]
