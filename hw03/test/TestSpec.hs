module TestSpec (spec) where

import MyLib

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

spec :: Spec
spec = do
{- Не уверен, что это вообще нужно тестировать, потому что используются функции стандартной библиотеки    
    describe "Eq instance for ChurchNumber" $ do
      it "Equality of two Zero values" $ do
        Zero == Zero `shouldBe` True

      it "Equality of two Succ values" $ do
        (Succ (Succ Zero)) == (Succ (Succ Zero)) `shouldBe` True

      it "Inequality of Zero and Succ" $ do
        Zero == (Succ Zero) `shouldBe` False

    describe "Ord instance for ChurchNumber" $ do
      it "Comparison of two Zero values" $ do
        compare Zero Zero `shouldBe` EQ

      it "Comparison of Zero and Succ" $ do
        compare Zero (Succ Zero) `shouldBe` LT

      it "Comparison of Succ and Zero" $ do
        compare (Succ Zero) Zero `shouldBe` GT

      it "Comparison of two Succ values" $ do
        compare (Succ (Succ Zero)) (Succ Zero) `shouldBe` GT

    describe "Num instance for ChurchNumber" $ do
      it "Addition of Zero and Succ" $ do
        (Zero + Succ (Succ Zero)) `shouldBe` (Succ (Succ Zero))

      it "Subtraction of Succ and Zero" $ do
        (Succ (Succ (Succ Zero)) - Zero) `shouldBe` (Succ (Succ (Succ Zero)))

      it "Multiplication of Succ values" $ do
        (Succ (Succ Zero) * Succ (Succ (Succ Zero))) `shouldBe` Succ (Succ (Succ (Succ (Succ (Succ (Zero))))))


    describe "Ix instance for ChurchNumber" $ do
      it "Range from Zero to Zero" $ do
        range (Zero, Zero) `shouldBe` [Zero]

      it "Range from Zero to Succ" $ do
        range (Zero, Succ (Succ Zero)) `shouldBe` [Zero, Succ Zero, Succ (Succ Zero)]

      it "Index Zero in the range" $ do
        index (Zero, Succ (Succ Zero)) Zero `shouldBe` 0

      it "Index Succ in the range" $ do
        index (Zero, Succ (Succ Zero)) (Succ (Succ Zero)) `shouldBe` 2

      it "In range Zero" $ do
        inRange (Zero, Succ (Succ Zero)) Zero `shouldBe` True

      it "Not in range Succ" $ do
        inRange (Zero, Succ Zero) (Succ (Succ Zero)) `shouldBe` False
-}

    describe "InOrder instance for Tree" $ do
      it "In-Order traversal of a single-node tree" $ do
        let tree = In (Node 42 [])
        show tree `shouldBe` "42"

      it "In-Order traversal of a tree with multiple nodes" $ do
        let tree = In (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])
        show tree `shouldBe` "2453514535"

    describe "PreOrder instance for Tree" $ do
      it "Pre-Order traversal of a single-node tree" $ do
        let tree = Pre (Node 42 [])
        show tree `shouldBe` "42"

      it "Pre-Order traversal of a tree with multiple nodes" $ do
        let tree = Pre (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])
        show tree `shouldBe` "12345"

    describe "PostOrder instance for Tree" $ do
      it "Post-Order traversal of a single-node tree" $ do
        let tree = Post (Node 42 [])
        show tree `shouldBe` "42"

      it "Post-Order traversal of a tree with multiple nodes" $ do
        let tree = Post (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])
        show tree `shouldBe` "24531"

    describe "ToCMYK instance for RGB" $ do
      it "Convert RGB (255, 0, 0) to CMYK" $ do
        toCMYK (UnsafeMkRGB 255 0 0) `shouldBe` Just (UnsafeMkCMYK 0 100 100 0)

      it "Invalid input RGB (256, 0, 0)" $ do
        toCMYK (UnsafeMkRGB 256 0 0) `shouldBe` Nothing

    describe "DToCMYK instance for [Int]" $ do
      it "Convert [0, 255, 255] to CMYK using DToCMYK" $ do
        toCMYK' dToCMYKList [0, 255, 255] `shouldBe` Just (UnsafeMkCMYK 100 0 0 0)

      it "Invalid input [-1, 255, 255] using DToCMYK" $ do
        toCMYK' dToCMYKList [-1, 255, 255] `shouldBe` Nothing

    describe "DToCMYK instance for RGB" $ do
      it "Convert RGB (0, 255, 255) to CMYK using DToCMYK" $ do
        toCMYK' dToCMYKRGB (UnsafeMkRGB 0 255 255) `shouldBe` Just (UnsafeMkCMYK 100 0 0 0)

      it "Invalid input RGB (0, 256, 255) using DToCMYK" $ do
        toCMYK' dToCMYKRGB (UnsafeMkRGB 0 256 255) `shouldBe` Nothing

    describe "nextDay" $ do
      it "Next day after Monday is Tuesday" $ do
        nextDay Monday `shouldBe` Tuesday

      it "Next day after Sunday is Monday" $ do
        nextDay Sunday `shouldBe` Monday

    describe "dayBefore" $ do
      it "Previous day before Tuesday is Monday" $ do
        dayBefore Tuesday `shouldBe` Monday

      it "Previous day before Monday is Sunday" $ do
        dayBefore Monday `shouldBe` Sunday

    describe "daysBeforeWeekend" $ do
      it "Days before nearest Saturday from Wednesday" $ do
        daysBeforeWeekend Wednesday `shouldBe` 3

      it "Days before nearest Saturday from Saturday" $ do
        daysBeforeWeekend Saturday `shouldBe` 0

    describe "GumRats instance for Monday" $ do
      it "Monday workout" $ do
        workout Chest `shouldBe` "Chest workout on Monday"

    describe "GumRats instance for Tuesday" $ do
      it "Tuesday workout" $ do
        workout Back `shouldBe` "Back workout on Tuesday"

    describe "GumRats instance for Wednesday" $ do
      it "Wednesday workout" $ do
        workout Shoulders `shouldBe` "Shoulder workout on Wednesday"

    describe "GumRats instance for Thursday" $ do
      it "Thursday workout" $ do
        workout Arms `shouldBe` "Arm workout on Thursday"

    describe "GumRats instance for Friday" $ do
      it "Friday workout" $ do
        workout Legs `shouldBe` "Leg workout on Friday"
