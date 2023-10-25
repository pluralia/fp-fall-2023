module TestSpec where

import Test.Hspec
import MyLib
import Data.Ix

spec :: Spec
spec = do
    -- 1 Task
    describe "ChurchNumber" $ do
        describe "Eq instance" $ do
            it "Zero == Zero" $
                Zero `shouldBe` Zero

            it "(Succ Zero) == (Succ Zero)" $
                Succ Zero `shouldBe` Succ Zero

            it "Zero /= (Succ Zero)" $
                Zero `shouldNotBe` Succ Zero

        describe "Ord instance" $ do
            it "Zero `compare` Zero == EQ" $
                Zero `compare` Zero `shouldBe` EQ

            it "Zero < (Succ Zero)" $
                Zero `shouldSatisfy` (< Succ Zero)

            it "(Succ Zero) > Zero" $
                Succ Zero `shouldSatisfy` (> Zero)

            it "(Succ (Succ Zero)) > (Succ Zero)" $
                Succ (Succ Zero) `shouldSatisfy` (> Succ Zero)

        describe "Num instance" $ do
            describe "(+)" $ do
                it "Zero + Zero == Zero" $ do
                    Zero + Zero `shouldBe` Zero

                it "Zero + (Succ Zero) == Succ Zero" $ do
                    Zero + Succ Zero `shouldBe` Succ Zero

                it "(Succ Zero) + (Succ Zero) == Succ (Succ Zero)" $ do
                    Succ Zero + Succ Zero `shouldBe` Succ (Succ Zero)

            describe "(*)" $ do
                it "Zero * Zero == Zero" $ do
                    Zero * Zero `shouldBe` Zero

                it "Zero * (Succ Zero) == Zero" $ do
                    Zero * Succ Zero `shouldBe` Zero

                it "(Succ Zero) * (Succ Zero) == Succ Zero" $ do
                    Succ Zero * Succ Zero `shouldBe` Succ Zero

            describe "abs" $ do
                it "abs Zero == Zero" $ do
                    abs Zero `shouldBe` Zero

                it "abs (Succ Zero) == Succ Zero" $ do
                    abs (Succ Zero) `shouldBe` Succ Zero

            describe "signum" $ do
                it "signum Zero == Zero" $ do
                    signum Zero `shouldBe` Zero

                it "signum (Succ Zero) == Succ Zero" $ do
                    signum (Succ Zero) `shouldBe` Succ Zero

            describe "fromInteger" $ do
                it "fromInteger 0 == Zero" $ do
                    fromInteger 0 `shouldBe` Zero

                it "fromInteger 1 == Succ Zero" $ do
                    fromInteger 1 `shouldBe` Succ Zero

                it "fromInteger 2 == Succ (Succ Zero)" $ do
                    fromInteger 2 `shouldBe` Succ (Succ Zero)

            describe "(-)" $ do
                it "Zero - Zero == Zero" $ do
                    Zero - Zero `shouldBe` Zero

                it "(Succ zero) - zero == Succ zero" $ do
                    Succ Zero - Zero `shouldBe` Succ Zero

                it "(succ zero) - succ zero == zero" $ do
                    Succ Zero - Succ Zero `shouldBe` Zero 

        describe "Ix instance" $ do
            describe "range" $ do
                it "returns an empty list when the first number is greater than the second" $ do
                    range (Succ Zero, Zero) `shouldBe` []

                it "returns a list with a single element when the first and second numbers are the same" $ do
                    range (Succ Zero, Succ Zero) `shouldBe` [Succ Zero]

                it "returns a list from n to m when the first number is less than the second" $ do
                    range (Zero, Succ (Succ Zero)) `shouldBe` [Zero, Succ Zero, Succ (Succ Zero)]

            describe "index" $ do
                it "returns 0 when the index is equal to the first number in the range" $ do
                    index (Zero, Succ Zero) Zero `shouldBe` 0

                it "returns 1 when the index is one more than the first number in the range" $ do
                    index (Zero, Succ Zero) (Succ Zero) `shouldBe` 1

                it "returns n when the index is n more than the first number in the range" $ do
                    index (Zero, Succ (Succ Zero)) (Succ (Succ Zero)) `shouldBe` 2

            describe "inRange" $ do
                it "returns False when the number is less than the first number in the range" $ do
                    inRange (Succ Zero, Succ (Succ Zero)) Zero `shouldBe` False

                it "returns True when the number is equal to the first number in the range" $ do
                    inRange (Succ Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` True

                it "returns True when the number is greater than the first number but less than or equal to the second number in the range" $ do
                    inRange (Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` True

-- 2 Task a

    describe "InOrder" $ do
        it "shows a single node correctly" $ do
            show (In (Node (1 :: Int) [])) `shouldBe` "1"

        it "shows a tree with multiple children correctly" $ do
            show (In (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [], Node (4 :: Int) []])) `shouldBe` "1 2 3 4"

        it "shows a complex tree correctly" $ do
            show (In (Node (1 :: Int) [Node (2 :: Int) [Node (4 :: Int) []], Node (3 :: Int) [Node (5 :: Int) [], Node (6 :: Int) []]])) `shouldBe` "1 2 4 3 5 6"

    describe "PreOrder" $ do
        it "shows a single node correctly" $ do
            show (Pre (Node (1 :: Int) [])) `shouldBe` "1"

        it "shows a binary tree correctly" $ do
            show (Pre (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) []])) `shouldBe` "1 2 3"

        it "shows a tree with multiple children correctly" $ do
            show (Pre (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [], Node (4 :: Int) []])) `shouldBe` "1 2 3 4"

        it "shows a complex tree correctly" $ do
            show (Pre (Node (1 :: Int) [Node (2 :: Int) [Node (4 :: Int) []], Node (3 :: Int) [Node (5 :: Int) [], Node (6 :: Int) []]])) `shouldBe` "1 2 4 3 5 6"

    describe "PostOrder" $ do
        it "shows a single node correctly" $ do
            show (Post (Node (1 :: Int) [])) `shouldBe` "1"

        it "shows a binary tree correctly" $ do
            show (Post (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) []])) `shouldBe` "2 3 1"

        it "shows a tree with multiple children correctly" $ do
            show (Post (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [], Node (4 :: Int) []])) `shouldBe` "2 3 4 1"

        it "shows a complex tree correctly" $ do
            show (Post (Node (1 :: Int) [Node (2 :: Int) [Node (4 :: Int) []], Node (3 :: Int) [Node (5 :: Int) [], Node (6 :: Int) []]])) `shouldBe` "4 2 5 6 3 1"

-- 2 Task b
    describe "Eq Tree" $ do
        it "checks equality of single node trees correctly" $ do
            Node (1 :: Int) [] `shouldBe` Node (1 :: Int) []

        it "checks equality of trees with multiple children correctly" $ do
            Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) []] `shouldBe` Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) []]

        it "checks inequality of trees correctly" $ do
            Node (1 :: Int) [Node (2 :: Int) []] `shouldNotBe` Node (1 :: Int) [Node (3 :: Int) []]

        it "checks equality of complex trees correctly" $ do
            Node (1 :: Int) [Node (2 :: Int) [Node (4 :: Int) []], Node (3 :: Int) [Node (5 :: Int) [], Node (6 :: Int) []]] `shouldBe` Node (1 :: Int) [Node (2 :: Int) [Node (4 :: Int) []], Node (3 :: Int) [Node (5 :: Int) [], Node (6 :: Int) []]]

-- 3 Task a
    describe "ToCMYK [Int]" $ do
        it "converts a valid list of integers to CMYK" $ do
            toCMYK ([75, 0, 75, 0] :: [Int]) `shouldBe` Just (UnsafeMkCMYK 75 0 75 0)

        it "returns Nothing for an invalid list of integers" $ do
            toCMYK ([300, 0, 75, 0] :: [Int]) `shouldBe` Nothing

        it "returns Nothing for a list of incorrect length" $ do
            toCMYK ([75, 0, 75] :: [Int]) `shouldBe` Nothing

    describe "ToCMYK RGB" $ do
        it "converts a valid RGB color to CMYK" $ do
            toCMYK (UnsafeMkRGB 255 255 255) `shouldBe` Just (UnsafeMkCMYK 0 0 0 0)

        it "returns Nothing for an invalid RGB color" $ do
            toCMYK (UnsafeMkRGB 300 255 255) `shouldBe` Nothing

        it "correctly converts black" $ do
            toCMYK (UnsafeMkRGB 0 0 0) `shouldBe` Just (UnsafeMkCMYK 0 0 0 100)

-- 3 Task b
    describe "dToCMYKList" $ do
        it "returns Just CMYK for valid input" $ do
            toCMYK' dToCMYKList [0, 50, 100, 0] `shouldBe` Just (UnsafeMkCMYK 0 50 100 0)

        it "returns Nothing for invalid input" $ do
            toCMYK' dToCMYKList [0, 50, 100, 101] `shouldBe` Nothing

        it "returns Nothing for list of incorrect length" $ do
            toCMYK' dToCMYKList [0, 50, 100] `shouldBe` Nothing

    describe "dToCMYKRGB" $ do
        it "returns Just CMYK for valid input" $ do
            toCMYK' dToCMYKRGB (UnsafeMkRGB 255 128 0) `shouldBe` Just (UnsafeMkCMYK 0 50 100 0)

        it "returns Nothing for invalid input" $ do
            toCMYK' dToCMYKRGB (UnsafeMkRGB 256 128 0) `shouldBe` Nothing

        it "returns Just CMYK for black color" $ do
            toCMYK' dToCMYKRGB (UnsafeMkRGB 0 0 0) `shouldBe` Just (UnsafeMkCMYK 0 0 0 100)

-- Task 5
    describe "nextDay" $ do
        it "returns Tuesday for Monday" $
            nextDay Monday `shouldBe` Tuesday

        it "returns Wednesday for Tuesday" $
            nextDay Tuesday `shouldBe` Wednesday

        it "returns Monday for Sunday" $
            nextDay Sunday `shouldBe` Monday

    describe "dayBefore" $ do
        it "returns Sunday for Monday" $
            dayBefore Monday `shouldBe` Sunday

        it "returns Saturday for Sunday" $
            dayBefore Sunday `shouldBe` Saturday

        it "returns Friday for Saturday" $
            dayBefore Saturday `shouldBe` Friday

    describe "daysBeforeWeekend" $ do
        it "returns 1 for Friday" $
            daysBeforeWeekend Friday `shouldBe` 1

        it "returns 0 for Saturday" $
            daysBeforeWeekend Saturday `shouldBe` 0

        it "returns 6 for Sunday" $
            daysBeforeWeekend Sunday `shouldBe` 6

-- Task 8
    describe "HoroscopeCompatibility" $ do
        let horoscope1 = MyHoroscope { mySign = "Aries", myPrediction = "You will have a great day!" }
        let horoscope2 = MyHoroscope { mySign = "Aries", myPrediction = "Be careful with finances today." }
        let horoscope3 = MyHoroscope { mySign = "Leo", myPrediction = "An old friend will contact you." }

        describe "compatible" $ do
            it "returns True for two Aries horoscopes" $
                compatible horoscope1 horoscope2 `shouldBe` True

            it "returns False for an Aries and a Leo horoscope" $
                compatible horoscope1 horoscope3 `shouldBe` False

        describe "compatibilityMessage" $ do
            it "returns a compatibility message for two Aries horoscopes" $
                compatibilityMessage horoscope1 horoscope2 `shouldBe` "The signs Aries and Aries are compatible."

            it "returns a special message for a Leo horoscope" $
                compatibilityMessage horoscope1 horoscope3 `shouldBe` "Think again..."