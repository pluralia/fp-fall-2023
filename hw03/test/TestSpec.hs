module TestSpec (spec) where

import MyLib
import Data.Bifunctor (Bifunctor (bimap))

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

spec :: Spec
spec = do
  -- lhint  дал замечаниz к скобкам для compare, но они нужны, потому что иначе 
  -- во время тестирования компилятор не понимает, к чему относится Succ без скобок
    describe "ChurchNumbers, Eq" $ do
      it "Two Zeros" $ do
        Zero == Zero `shouldBe` True

      it "Two ChurchNumbers equal" $ do
        Succ (Succ (Succ Zero)) == Succ (Succ (Succ Zero)) `shouldBe` True

      it "Two ChurchNumbers unequal" $ do
        Succ (Succ (Succ Zero)) == Succ (Succ Zero) `shouldBe` False

    describe "ChurchNumbers, Ord" $ do
      it "Two Zeros" $ do
        compare Zero Zero `shouldBe` EQ

      it "Zero and Succ" $ do
        compare Zero (Succ (Zero)) `shouldBe` LT

      it "Succ and Zero" $ do
        compare (Succ (Succ (Zero))) Zero `shouldBe` GT

      it "Two Succ" $ do
        compare (Succ (Succ (Zero))) (Succ (Zero)) `shouldBe` GT

    describe "ChurchNumbers, Num" $ do
      it "Zero + Succ" $ do
        (Zero + Succ (Succ (Zero))) `shouldBe` Succ (Succ (Zero))

      it "Succ - Zero" $ do
        (Succ (Succ Zero) - Zero) `shouldBe` Succ (Succ Zero)

      it "Succ * Succ" $ do
        (Succ (Succ Zero) * Succ (Succ Zero)) `shouldBe` Succ (Succ (Succ (Succ (Zero))))

    -- describe "Trees"...

    -- describe "Colors"...

    describe "Next Day" $ do
      it "Tuesday -> Wednesday" $ do
        nextDay Tue `shouldBe` Wed

      it "Sunday -> Monday" $ do
        nextDay Sun `shouldBe` Mon

    describe "Previous day" $ do
      it "Wednesday <- Tuesday" $ do
        dayBefore Wed `shouldBe` Tue

      it "Monday <- Sunday" $ do
        dayBefore Mon `shouldBe` Sun

    describe "Days to Saturday" $ do
      it "Days to Saturday from Wednesday" $ do
        daysBeforeWeekend Wed `shouldBe` 3

      it "Days to Saturday from Saturday" $ do
        daysBeforeWeekend Sat `shouldBe` 0
