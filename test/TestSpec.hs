
module TestSpec where

import Test.Hspec
import MyLib
import Data.Ix (inRange, index, range)
import Data.Bifunctor 

spec :: Spec
spec = do

-- совсем не смогла пока поправить ворниг про [-Wtype-defaults] для (show .), но очень стараюсь это починить!!

-- | Number 1

    let one     = Succ Zero
    let five    = Succ (Succ (Succ (Succ one)))
    let eight   = Succ (Succ (Succ (Succ (Succ (Succ (Succ one))))))


    describe "instances for ChurchNumbers" $ do
        it "Eq" $ do
            Zero == Zero `shouldBe` True
        it "Eq" $ do
            one /= five `shouldBe` True
        it "Eq" $ do
            one == one `shouldBe` True
        it "Eq" $ do
            five == eight `shouldBe` False
        it "Eq" $ do
            five /= five `shouldBe` False

        it "Ord" $ do
            Zero <= Zero `shouldBe` True
        it "Ord" $ do
            one < five `shouldBe` True
        it "Ord" $ do
            one >= one `shouldBe` True
        it "Ord" $ do
            five > eight `shouldBe` False
        
        it "Num" $ do
            Zero + Zero `shouldBe` Zero
        it "Num" $ do
            one * five `shouldBe` five
        it "Num" $ do
            one - one `shouldBe` Zero
        it "Num" $ do
            abs five `shouldBe` five
        it "Num" $ do
            signum Zero `shouldBe` Zero
        it "Num" $ do
            signum eight `shouldBe` Succ Zero      
        it "Num" $ do
            (8 :: ChurchNumber) `shouldBe` eight

        it "Ix" $ do
            range (one, five) `shouldBe` [one, one + one, one + one + one, five - one, five]
        it "Ix" $ do
            index (five, eight) (five + one) `shouldBe` 1
        it "Ix" $ do
            inRange (one, eight) five `shouldBe` True
        it "Ix" $ do
            inRange (one, eight) eight `shouldBe` True
        it "Ix" $ do
            inRange (one, five) eight `shouldBe` False
        
-- | Number 2
    let myTree1 = Node 2 [Node 3 [Node 5 []], Node 1 []]
    let myTree2 = Node 2 [Node 3 [Node 5 []], Node 1 [], Node 4 [], Node 0 [Node 6 [], Node 7 []]]
    let myTree3 = Node 2 [Node 3 [Node 5 []], Node 1 [], Node 4 [], Node 6 [], Node 7 [Node 8 [], Node 9 []]]

    describe "instances for Tree" $ do
        it "In-order" $ do
            (show . In $ myTree1) `shouldBe` "5 3 2 1"
        it "In-order" $ do
            (show . In $ myTree2) `shouldBe` "5 3 2 1 2 4 2 6 0 7"
        it "In-order" $ do
            (show . In $ myTree3) `shouldBe` "5 3 2 1 2 4 2 6 2 8 7 9"
        
        it "Pre-order" $ do
            (show . Pre $ myTree1) `shouldBe` "2 3 5 1"
        it "Pre-order" $ do
            (show . Pre $ myTree2) `shouldBe` "2 3 5 1 4 0 6 7"
        it "Pre-order" $ do
            (show . Pre $ myTree3) `shouldBe` "2 3 5 1 4 6 7 8 9"
        
        it "Post-order" $ do
            (show . Post $ myTree1) `shouldBe` "5 3 1 2"
        it "Post-order" $ do
            (show . Post $ myTree2) `shouldBe` "5 3 1 4 6 7 0 2"
        it "Post-order" $ do
            (show . Post $ myTree3) `shouldBe` "5 3 1 4 6 8 9 7 2"
        
        it "Eq" $ do
            myTree1 == myTree2 `shouldBe` False
        it "Eq" $ do
            myTree1 /= myTree3 `shouldBe` True
        it "Eq" $ do
            myTree3 == myTree3 `shouldBe` True

-- | Number 3
    describe "instances to CMYK" $ do
        it "for [Int]" $ do
            toCMYK ([1, 2, 3, 4] :: [Int]) `shouldBe` Just (UnsafeMkCMYK {cyan = 1, magenta = 2, yellow = 3, black = 4})
        it "for [Int]" $ do
            toCMYK purpure `shouldBe` Just (UnsafeMkCMYK {cyan = 50, magenta = 60, yellow = 0, black = 0})
        it "for [Int]" $ do
            toCMYK ([200, 2, 3, 4] :: [Int]) `shouldBe` Nothing
        it "for RGB" $ do
            toCMYK purpureRGB `shouldBe` Just (UnsafeMkCMYK {cyan = 0, magenta = 100, yellow = 0, black = 50})
        it "for [Int]" $ do
            purpureFromInt `shouldBe` purpureFromInt'
        it "for RGB" $ do
            purpureFromRGB `shouldBe` purpureFromRGB'
    
-- | Number 5
    describe "WeekEnd!" $ do
        it "instance Enum" $ do
            (toEnum 5 :: Day) `shouldBe` Fri
        it "instance Enum" $ do
            (toEnum 1 :: Day) `shouldBe` Mon
        it "instance Enum" $ do
            (toEnum 7 :: Day) `shouldBe` Sun
        it "instance Enum" $ do
            fromEnum Sat `shouldBe` 6
        it "instance Enum" $ do
            fromEnum Tue `shouldBe` 2
        it "instance Enum" $ do
            fromEnum Wed `shouldBe` 3
        
        it "next day is ..." $ do
            nextDay Mon `shouldBe` Tue
        it "next day is ..." $ do
            nextDay Fri `shouldBe` Sat
        it "next day is ..." $ do
            nextDay Sun `shouldBe` Mon
        
        it "day before is ..." $ do
            dayBefore Mon `shouldBe` Sun
        it "day before is ..." $ do
            dayBefore Sat `shouldBe` Fri
        it "day before is ..." $ do
            dayBefore Tue `shouldBe` Mon
        
        it "days for Sat ..." $ do
            daysBeforeWeekend Mon `shouldBe` 5
        it "days for Sat ..." $ do
            daysBeforeWeekend Sat `shouldBe` 0
        it "days for Sat ..." $ do
            daysBeforeWeekend Tue `shouldBe` 4
    
-- | Number 6
    let testList = Cons "a" (Cons "b" Nil)

    describe "Functor" $ do
        it "fmap for List" $ do
            fmap (=="a") testList `shouldBe` Cons True (Cons False Nil)
        it "fmap for List" $ do
            fmap (=="b") testList `shouldBe` Cons False (Cons True Nil)
        it "fmap for List" $ do
            fmap (=="c") testList `shouldBe` Cons False (Cons False Nil)

        it "fmap for Tree" $ do
            fmap (*2) myTree1 `shouldBe` Node {value = 4, children = 
                [Node {value = 6, children = 
                    [Node {value = 10, children = []}]}, Node {value = 2, children = []}
                    ]}
            fmap (<5) myTree2 `shouldBe` Node {value = True, children = 
                [Node {value = True, children = 
                    [Node {value = False, children = []}]}
                , Node {value = True, children = []}
                , Node {value = True, children = []}
                , Node {value = True, children = 
                    [Node {value = False, children = []},Node {value = False, children = []}]}
                ]}
        
    describe "Functor for Pair" $ do
        it "Pair" $ do
            fmap (*5) (Pair (1 :: Int) (10 :: Int)) `shouldBe` Pair (1 :: Int) (50 :: Int)
        it "Pair" $ do
            fmap (+2) (Pair "10" (8 :: Int)) `shouldBe` Pair "10" (10 :: Int)
        it "Pair" $ do
            fmap (==2) (Pair (1 :: Int) (10 :: Int)) `shouldBe` Pair (1 :: Int) False

-- | Number 7
    describe "BiFunctor" $ do
        it "Pair" $ do
            first (*2) (Pair (1 :: Int) (2 :: Int)) `shouldBe` Pair (2 :: Int) (2 :: Int)
        it "Pair" $ do
            second (+5) (Pair (10 :: Int) (20 :: Int)) `shouldBe` Pair (10 :: Int) (25 :: Int)
        it "Pair" $ do
            first (++ "!") (Pair "You" "Me") `shouldBe` Pair "You!" "Me"
        it "Pair" $ do
            second (++ "?") (Pair "You" "Me") `shouldBe` Pair "You" "Me?"
            
-- | Number 8
    describe "my function: Smiler" $ do
        it "sad emotion" $ do
            giveSmile (-100 :: Int) `shouldBe` "(T_T)"
        it "neitral emotion" $ do
            giveSmile (0 :: Int) `shouldBe` "(-_-)"
        it "surprised emotion" $ do
            giveSmile (78 :: Int) `shouldBe` "(o_o)"
        it "happy emotion" $ do
            giveSmile (100 :: Int) `shouldBe` "(^_^)"