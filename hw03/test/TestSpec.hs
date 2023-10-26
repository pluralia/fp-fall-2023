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
        compare Zero (Succ Zero) `shouldBe` LT

      it "Succ and Zero" $ do
        compare (Succ (Succ Zero)) Zero `shouldBe` GT

      it "Two Succ" $ do
        compare (Succ (Succ Zero)) (Succ Zero) `shouldBe` GT

    describe "ChurchNumbers, Num" $ do
      it "Zero + Succ" $ do
        (Zero + Succ (Succ Zero)) `shouldBe` Succ (Succ Zero)

      it "Succ - Zero" $ do
        (Succ (Succ Zero) - Zero) `shouldBe` Succ (Succ Zero)

      it "Succ * Succ" $ do
        (Succ (Succ Zero) * Succ (Succ Zero)) `shouldBe` Succ (Succ (Succ (Succ Zero)))

      it "abs four" $ do
        abs (Succ (Succ (Succ (Succ Zero)))) `shouldBe` Succ (Succ (Succ (Succ Zero)))

      it "signum Zero" $ do
        signum Zero `shouldBe` Zero
      
      it "signum of four" $ do
        signum (Succ (Succ (Succ (Succ Zero)))) `shouldBe` Succ Zero

    describe "Trees traversal" $ do
      it "In-Order" $ do
        let tree = In (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [Node (4 :: Int) [], Node (5 :: Int) []]])
        show tree `shouldBe` "24531"

      it "Pre-Order" $ do
        let tree = Pre (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [Node (4 :: Int) [], Node (5 :: Int) []]])
        show tree `shouldBe` "12345"

      it "Post-Order" $ do
        let tree = Post (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [Node (4 :: Int) [], Node (5 :: Int) []]])
        show tree `shouldBe` "24531"

    describe "Eq Tree" $ do
      let myTree1 = Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) []]
      let myTree2 = Node (4 :: Int) [Node (5 :: Int) [], Node (7 :: Int) [Node (6 :: Int) []]]
      let myTree3 = Node (4 :: Int) [Node (5 :: Int) [], Node (6 :: Int) [Node (7 :: Int) []]]
      let myTree4 = myTree1

      it "Different size" $ do
        myTree1 == myTree2 `shouldBe` False

      it "Different order" $ do
        myTree2 == myTree3 `shouldBe` False
      
      it "Eq trees" $ do
        myTree1 == myTree4 `shouldBe` True

      it "Same tree" $ do
        myTree3 == myTree3 `shouldBe` True

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
