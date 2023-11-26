{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import MyHw3
  ( ChurchNumber (..),
    Day (Monday', Saturday', Sunday', Tuesday'),
    Either' (..),
    List (..),
    Tree (Node),
    dayBefore,
    daysBeforeWeekend,
    nextDay,
  )
import MyHw4
  ( average,
    buildQuery,
    evenodd,
    fromListL,
    fromListR,
    gcContent,
    nubOrd,
    padZero,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  -- for tests
  let one = Succ Zero
  let two = Succ one
  let three = Succ two
  let four = Succ three
  let five = Succ four

  -- HW3

  describe "Eq ChurchNumber'" $ do
    it "go" $ do
      one == three `shouldBe` False
      one == four `shouldNotBe` True
      one /= three `shouldBe` True
      one == one `shouldBe` True

  describe "Ord ChurchNumber'" $ do
    it "go" $ do
      one >= three `shouldBe` False
      one <= four `shouldNotBe` False
      one >= three `shouldBe` False

  describe "Num ChurchNumber'" $ do
    it "go" $ do
      one + three `shouldNotBe` (five :: ChurchNumber)
      one + three `shouldBe` (four :: ChurchNumber)
      three - one `shouldBe` (two :: ChurchNumber)
      three * one `shouldBe` (three :: ChurchNumber)
      two * two `shouldBe` (four :: ChurchNumber)
      negate two `shouldBe` (Zero :: ChurchNumber)
      signum two `shouldBe` (Succ Zero :: ChurchNumber)
      2 `shouldBe` (two :: ChurchNumber)
      2 `shouldBe` (two :: ChurchNumber)

  describe "Enum Day'" $ do
    it "go" $ do
      toEnum 0 `shouldBe` (Monday' :: Day)
      fromEnum Monday' `shouldBe` (0 :: Int)

      nextDay Monday' `shouldBe` (Tuesday' :: Day)
      nextDay Sunday' `shouldBe` (Monday' :: Day)

      dayBefore Sunday' `shouldBe` (Saturday' :: Day)
      dayBefore Monday' `shouldBe` (Sunday' :: Day)

      daysBeforeWeekend Saturday' `shouldBe` (0 :: Int)
      daysBeforeWeekend Monday' `shouldBe` (5 :: Int)

  describe "List Functor" $ do
    it "go" $ do
      fmap (+ 1) (1 `Cons` (2 `Cons` (3 `Cons` Nil))) `shouldBe` ((2 `Cons` (3 `Cons` (4 `Cons` Nil))) :: List Int)

    it "go" $ do
      fmap (+ 1) Nil `shouldBe` (Nil :: List Int)

  describe "Tree Functor" $ do
    it "go" $ do
      fmap (+ 1) (Node 1 [Node 2 [], Node 3 []] :: Tree Int) `shouldBe` (Node 2 [Node 3 [], Node 4 []] :: Tree Int)

    -- подсмотрено у Артемия
    it "go" $ do
      fmap (map (+ 1)) (Node [1, 2, 3] [] :: Tree [Int]) `shouldBe` (Node [2, 3, 4] [] :: Tree [Int])

  describe "Either' BiFunctor" $ do
    it "go" $ do
      bimap (+ 10) (* 10) (Left' 4) `shouldBe` (Left' 14 :: Either' Int Int)

    it "go" $ do
      bimap (+ 10) (* 10) (Right' 4) `shouldBe` (Right' 40 :: Either' Int Int)

  -- не могу скомпилить этот тест. Подскажи, пжлтс, в чем дело может быть ? :(

  -- describe "Pair' BiFunctor" $ do
  --   it "go" $
  --     bimap (map (++ "!")) (* 2) (Pair ["hello", "world"] [1, 2, 3, 4] :: Pair [String] [Int]) `shouldBe` (Pair ["hello!", "world!"] ([2, 4, 6, 8] :: [Int]) :: Pair [String] [Int])

  -- HW4

  describe "test padZero'" $ do
    let str1 = ("hh" :: T.Text)
    it "go" $ do
      padZero str1 2 `shouldBe` ("hh" :: T.Text)
    it "go" $ do
      padZero str1 5 `shouldBe` ("000hh" :: T.Text)

  describe "test evenodd'" $ do
    it "go" $ do
      evenodd ([1, 2, 3, 4] :: [Int]) `shouldBe` (([1, 3], [2, 4]) :: ([Int], [Int]))
    it "go" $ do
      evenodd ([1, 2, 3] :: [Int]) `shouldBe` (([1, 3], [2]) :: ([Int], [Int]))
    it "go" $ do
      evenodd ([] :: [Int]) `shouldBe` (([], []) :: ([Int], [Int]))

  describe "test average" $ do
    let eps = (1e-4 :: Double)
    it "go" $ do
      let v = V.fromList ([1 .. 10] :: [Double])
      let res = average v
      (abs (res - 5.5) < eps :: Bool) `shouldBe` True

    it "go" $ do
      let v = V.fromList ([-3 .. 3] :: [Double])
      let res = average v

      (abs res < eps :: Bool) `shouldBe` True

    it "go" $ do
      let v = V.fromList ([] :: [Double])
      let res = average v
      res `shouldBe` 0

  describe "test gc'" $ do
    let eps = (1e-4 :: Double)

    let seq1 = ("GCADCGGC" :: String)
    let seq2 = ("AB" :: String)
    let seq3 = ("GC" :: String)

    let seqT1 = (T.pack seq1 :: T.Text)
    let seqT2 = (T.pack seq2 :: T.Text)
    let seqT3 = (T.pack seq3 :: T.Text)

    it "go" $ do
      let res = gcContent seqT1
      (abs (res - 0.75) < eps :: Bool) `shouldBe` True

    it "go" $ do
      let res = gcContent seqT2
      (abs (res - 0) < eps :: Bool) `shouldBe` True

    it "go" $ do
      let res = gcContent seqT3
      (abs (res - 1) < eps :: Bool) `shouldBe` True


-- :p -> усталь писать тесты :(
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
    
-- :p -> усталь писать тесты :(
-- Task 10
  describe "nubOrd" $ do
    it "check correctness" $ do
      let lst1 = [1, 2, 1, 2, 1, 2, 3] :: [Int]
      let lst2 = [1..1000] :: [Int]
      let lst3 = ['a', 'a', 'b', 'a', 'd', 'f'] :: [Char]
      nubOrd lst1 `shouldBe` [1, 2, 3]
      nubOrd lst2 `shouldBe` [1..1000]
      nubOrd lst3 `shouldBe` ['a', 'b', 'd', 'f']


-- :p -> усталь писать тесты :(
-- Task 11
  describe "query parameters" $ do
    it "check correctness" $ do
      let map1 = M.fromList [("a", "1"), ("b", "2"), ("c", "hello")]
      let map2 = M.fromList [("Thank", "you"), ("for", "reviewing")]
      let map3 = M.empty
      buildQuery map1 `shouldBe` "a=1&b=2&c=hello"
      buildQuery map2 `shouldBe` "Thank=you&for=reviewing"
      buildQuery map3 `shouldBe` ""