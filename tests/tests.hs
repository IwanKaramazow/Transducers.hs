module Main where

import Transducers
import Test.QuickCheck
import Test.Hspec
import Data.List (intercalate)
import Data.Char

inc x = x + 1

xform1 = (xfilter odd) . (xmap inc)

lessThenThree :: String -> Bool
lessThenThree s = length s < 3


stringToUpper :: String -> String
stringToUpper s = map toUpper s

xform2 = (xfilter lessThenThree) . (xmap stringToUpper)
  
main :: IO ()
main = hspec $ do
  describe "Transducers" $ do
    it "should be able to transform a list and sum the values" $ do
      transduce xform1 (+) 100 [0..4] `shouldBe` 106
    it "should transform lists without reducingFn & start value" $ do
      xlist (xmap inc) [0..4] `shouldBe` [1..5]
    it "should transform" $ do
      xlist xform2 ["testtest", "don't show", "hm"] `shouldBe` ["HM"]

