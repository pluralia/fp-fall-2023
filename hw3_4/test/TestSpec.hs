{-# LANGUAGE OverloadedStrings #-}
module TestSpec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Data.Ix ( Ix(inRange, range, index) )
import Data.Bifunctor (Bifunctor (bimap))

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

import MyHw3
    ( Either'(..),
      Pair(..),
      Tree(Node),
      List(..),
      Day(Saturday, Thursday, Tuesday, Monday, Sunday, Wednesday),
      ChurchNumber(..),
      nextDay,
      dayBefore,
      daysBeforeWeekend )

import MyHw4
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
