{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

-- import Test.Hspec ( describe, it, shouldBe, Spec )
-- import Data.Ix ( Ix(inRange, range, index) )
import Data.Bifunctor (Bifunctor (bimap))
-- import qualified Data.Vector as V
-- import qualified Data.Map.Strict as M

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
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

-- import MyHw4
--     ( padZero,
--       evenodd,
--       average,
--       gcContent,
--       fromListL,
--       fromListR,
--       nubOrd,
--       buildQuery )

spec :: Spec
spec = do
  -- for tests
  let one = Succ Zero
  let two = Succ one
  let three = Succ two
  let four = Succ three
  let five = Succ four

  -- HW3

  -- Task 1
  --
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

