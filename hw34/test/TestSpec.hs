{-# LANGUAGE OverloadedStrings #-}
module TestSpec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Data.Ix ( Ix(inRange, range, index) )
import Data.Bifunctor (Bifunctor (bimap))

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

import MyHW3
    ( Either'(..),
      Pair(..),
      Tree(Node),
      List(..),
      Day(Saturday, Thursday, Tuesday, Monday, Sunday, Wednesday),
      ChurchNumber(..),
      nextDay,
      dayBefore,
      daysBeforeWeekend )

import MyHW4
    ( padZero,
      evenodd,
      average,
      gcContent,
      fromListL,
      fromListR,
      nubOrd,
      buildQuery )

spec :: Spec
spec = do

-- Predefined data
  let one = Succ Zero
  let two = Succ one
  let three = Succ two
  let four = Succ three
  let five = Succ four

-- HW3

-- Task 1
--
  describe "Eq ChurchNumber'" $ do
    it "check methods" $ do
      Zero == Zero `shouldBe` True
      four == five `shouldBe` False
      one == Zero `shouldBe` False
      five /= five `shouldBe` False

  describe "Ord ChurchNumber'" $ do
    it "check methods" $ do
      Zero < three `shouldBe` True
      one < Zero `shouldBe` False
      two > two `shouldBe` False
      four <= five `shouldBe` True

  describe "Num ChurchNumber" $ do
    it "check methods" $ do
      one + three `shouldBe` four
      one + Zero `shouldBe` one
      Zero + Zero `shouldBe` Zero

      one - three `shouldBe` Zero
      five - two `shouldBe` three
      Zero - one `shouldBe` Zero
      four - Zero `shouldBe` four

      Zero * one `shouldBe` Zero
      five * Zero `shouldBe` Zero
      two * two `shouldBe` four

      negate two `shouldBe` Zero
      abs two `shouldBe` two

      signum Zero `shouldBe` Zero
      signum four `shouldBe` one

      fromInteger 0 `shouldBe` Zero
      fromInteger 3 `shouldBe` three

  describe "Ix ChurchNumber" $ do
    it "check range" $ do
      range (Zero, Zero) `shouldBe` ([Zero] :: [ChurchNumber])
      range (Zero, two) `shouldBe` ([Zero, one, two] :: [ChurchNumber])
      range (one, five) `shouldBe` ([one, two, three, four, five] :: [ChurchNumber])

    it "check index" $ do
      index (one, five) four `shouldBe` 3
      index (one, one) one `shouldBe` 0

    it "check inRange" $ do
      inRange (one, five) four `shouldBe` True
      inRange (two, three) four `shouldBe` False
      inRange (Zero, Zero) five `shouldBe` False

-- Task 5
  describe "Day" $ do
    it "check constructor" $ do
      Monday `shouldBe` Monday

  describe "Enum Day" $ do
    it "check toEnum" $ do
      toEnum 1 `shouldBe` Monday
      toEnum 7 `shouldBe` Sunday

    it "check fromEnum" $ do
      fromEnum Monday `shouldBe` 1
      fromEnum Sunday `shouldBe` 7

  describe "functions for Day" $ do
    it "check nextDay" $ do
      nextDay Wednesday `shouldBe` Thursday
      nextDay Sunday `shouldBe` Monday

    it "check dayBefore" $ do
      dayBefore Wednesday `shouldBe` Tuesday
      dayBefore Monday `shouldBe` Sunday

    it "check daysBeforeWeekend" $ do
      daysBeforeWeekend Wednesday `shouldBe` 3
      daysBeforeWeekend Saturday `shouldBe` 0

-- Task 6

  describe "Functor" $ do
    it "Functor List" $ do
      let strList = Cons "j" $ Cons "a" $ Cons "v" $ Cons "a" Nil :: List String
      let intList = Cons 3 $ Cons 2 $ Cons 1 Nil :: List Int
      fmap (== "v") strList `shouldBe` (Cons False $ Cons False $ Cons True $ Cons False Nil :: List Bool)
      fmap (+ 1) intList `shouldBe` (Cons 4 $ Cons 3 $ Cons 2 Nil :: List Int)

    it "Functor Tree" $ do
      let intTree = Node 2 [Node 1 [], Node 2 [Node 3 []]] :: Tree Int
      let strTree = Node "j" [Node "a" [], Node "v" [], Node "a" [Node "!" []]]
      fmap (1 +) intTree `shouldBe` (Node 3 [Node 2 [], Node 3 [Node 4 []]] :: Tree Int)
      fmap (++ "*") strTree `shouldBe` Node "j*" [Node "a*" [], Node "v*" [], Node "a*" [Node "!*" []]]

    it "Functor Pair" $ do
      let intPair = Pair (1 :: Int) (1 :: Int)
      let strPair = Pair "Haskell" "Language" :: Pair String String
      fmap (3 *) intPair `shouldBe` Pair (1 :: Int) (3 :: Int)
      fmap (++ "!") strPair `shouldBe` (Pair "Haskell" "Language!" :: Pair String String)

-- Task 7

  describe "Bifunctor" $ do
    it "Bifunctor Either'" $ do
      bimap (1 +) (2 +) (Left' 1 :: Either' Int Int) `shouldBe` Left' (2 :: Int)
      bimap (* 3) (* 2) (Right' 1 :: Either' Int Int) `shouldBe` Right' (2 :: Int)
      bimap (++ "^-^") (* 1) (Left' "hw3 " :: Either' String Int) `shouldBe` Left' "hw3 ^-^"

    it "Bifunctor Pair'" $ do
      bimap pred lines (Pair 1 "ghci" :: Pair Int String) `shouldBe` (Pair 0 ["ghci"] :: Pair Int [String])
      bimap (* 3) (* 2) (Pair 1 1 :: Pair Int Int) `shouldBe` (Pair 3 2 :: Pair Int Int)

-- HW4

-- Task 1
  describe "padZero" $ do
    it "check correctness" $ do
      padZero "HSE University" 20 `shouldBe` "000000HSE University" 
      padZero "HSE University" 2 `shouldBe` "HSE University"  

-- Task 3
  describe "evenodd" $ do
    it "check correctness" $ do
      let lst = [1, 2, 3, 4, 5, 6] :: [Int]
      evenodd lst `shouldBe` (([1, 3, 5], [2, 4, 6]) :: ([Int], [Int]))
      evenodd ([] :: [Int]) `shouldBe` (([], []) :: ([Int], [Int]))
      evenodd ([1] :: [Int]) `shouldBe` (([1], []) :: ([Int], [Int]))

-- Task 4
  describe "average" $ do
    it "check correctness" $ do
      let v1 = V.fromList [1, 2, 3, -3, -2, -1] :: V.Vector Double
      let v2 = V.fromList [] :: V.Vector Double
      let v3 = V.fromList [1..100] :: V.Vector Double

      average v1 `shouldBe` (0 :: Double)
      average v2 `shouldBe` (0 :: Double)
      average v3 `shouldBe` (50.5 :: Double)

-- Task 5
  describe "gcContent" $ do
    it "check correctness" $ do
      let seq1 = "ATGCATGCATGC" 
      let seq2 = "AATTAATTAATT"
      gcContent seq1 `shouldBe` (0.5 :: Double)
      gcContent seq2 `shouldBe` (0 :: Double)

-- Task 9
  describe "M.fromList" $ do
    let emptyLst = [] :: [(Int, Int)]
    let lst1 = [(1, 'a'), (2, 'b'), (3, 'c'), (2, 'd')] :: [(Int, Char)]
    it "fromListL" $ do
      fromListL emptyLst `shouldBe` (M.empty :: M.Map Int Int)
      fromListL lst1 `shouldBe` M.fromList [(1, 'a'), (2, 'd'), (3, 'c')]
    
    it "fromListL" $ do
      fromListR emptyLst `shouldBe` (M.empty :: M.Map Int Int)
      fromListR lst1 `shouldBe` M.fromList [(1, 'a'), (2, 'b'), (3, 'c')]
    
-- Task 10
  describe "nubOrd" $ do
    it "check correctness" $ do
      let lst1 = [1, 2, 1, 2, 1, 2, 3] :: [Int]
      let lst2 = [1..1000] :: [Int]
      let lst3 = ['a', 'a', 'b', 'a', 'd', 'f'] :: [Char]
      nubOrd lst1 `shouldBe` [1, 2, 3]
      nubOrd lst2 `shouldBe` [1..1000]
      nubOrd lst3 `shouldBe` ['a', 'b', 'd', 'f']

-- Task 11
  describe "query parameters" $ do
    it "check correctness" $ do
      let map1 = M.fromList [("a", "1"), ("b", "2"), ("c", "hello")]
      let map2 = M.fromList [("Thank", "you"), ("for", "reviewing")]
      let map3 = M.empty
      buildQuery map1 `shouldBe` "a=1&b=2&c=hello"
      buildQuery map2 `shouldBe` "Thank=you&for=reviewing"
      buildQuery map3 `shouldBe` ""