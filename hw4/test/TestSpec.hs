{-# LANGUAGE OverloadedStrings #-}
module TestSpec where

import Test.Hspec
import MyLib 
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

spec :: Spec
spec = do
  describe "padZero" $ do
    it "pads another string with zeros" $
      padZero "Hello" 10 `shouldBe` "00000Hello" 
    it "does not pad a short string" $
      padZero "Short" 3 `shouldBe` "Short"

  describe "evenodd" $ do
    it "check correctness" $ do
        let lst = [1, 2, 3, 4, 5, 6] :: [Int]
        evenodd lst `shouldBe` (([1, 3, 5], [2, 4, 6]) :: ([Int], [Int]))
    it "handles empty list" $ do
        evenodd ([] :: [Int]) `shouldBe` (([], []) :: ([Int], [Int]))

  describe "average" $ do
    it "handles empty vector" $ do
        let vec = V.empty :: V.Vector Double
        average vec `shouldBe` (0 :: Double) 
    it "calculates average of a vector with positive values" $ do
        let vec = V.fromList [1.0, 2.0, 3.0, 4.0, 5.0] :: V.Vector Double
        average vec `shouldBe` (3.0 :: Double)
    it "calculates average of a vector with negative values" $ do
        let vec = V.fromList [-2.0, -4.0, -6.0, -8.0, -10.0] :: V.Vector Double
        average vec `shouldBe` (-6.0 :: Double)

  describe "gcContent" $ do
    it "check with all GC" $ do
     let seq4 = "GCGCGCGC"
     gcContent seq4 `shouldBe` (1 :: Double)
    it "check correctness with no GC content" $ do
        let seq2 = "AABBAABBAABB"
        gcContent seq2 `shouldBe` (0 :: Double)

  describe "M.fromList with different handling of duplicate keys" $ do
    let emptyLst = [] :: [(Int, Int)]
    let lst1 = [(1, 10), (2, 20), (3, 30), (2, 40)] :: [(Int, Int)]
    it "fromListL (retains last value for duplicate keys)" $ do
        fromListL emptyLst `shouldBe` (M.empty :: M.Map Int Int)
        fromListL lst1 `shouldBe` M.fromList [(1, 10), (2, 40), (3, 30)]     
    it "fromListR (retains first value for duplicate keys)" $ do
        fromListR emptyLst `shouldBe` (M.empty :: M.Map Int Int)
        fromListR lst1 `shouldBe` M.fromList [(1, 10), (2, 20), (3, 30)]
  
  describe "nubOrd" $ do
    it "check correctness with multiple duplicates" $ do
        let lst1 = [1, 2, 1, 2, 1, 2, 3] :: [Int]
        nubOrd lst1 `shouldBe` [1, 2, 3]
    it "check correctness with a range of integers" $ do
        let lst2 = [1..1000] :: [Int]
        nubOrd lst2 `shouldBe` [1..1000]
    it "check correctness with character list" $ do
        let lst3 = ['a', 'a', 'b', 'a', 'd', 'f'] :: [Char]
        nubOrd lst3 `shouldBe` ['a', 'b', 'd', 'f']
    it "check correctness with an empty list" $ do
        let lst4 = [] :: [Int]
        nubOrd lst4 `shouldBe` ([] :: [Int])
    it "check correctness with a single element list" $ do
        let lst5 = [42] :: [Int]
        nubOrd lst5 `shouldBe` [42]
    it "check correctness with a list of Either Int Char" $ do
        let lst6 = [Left 1, Left 2, Left 3, Right 'a', Right 'b', Right 'c', Left 1, Right 'a'] :: [Either Int Char]
        nubOrd lst6 `shouldBe` [Left 1, Left 2, Left 3, Right 'a', Right 'b', Right 'c']





