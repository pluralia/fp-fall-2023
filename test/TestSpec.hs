module TestSpec (spec) where

import           Data.Functor.Identity
import           MyHW8
import           Test.Hspec


spec :: Spec
spec = do

-- Task 7

    describe "pythagoreanTriple" $ do

        it "check correctness" $ do
            pythagoreanTriple 3 `shouldBe` ([] :: [(Int, Int, Int)])
            pythagoreanTriple 5 `shouldBe` ([(3, 4, 5)] :: [(Int, Int, Int)])
            pythagoreanTriple 10 `shouldBe` ([(3,4,5), (6,8,10)] :: [(Int, Int, Int)])

-- Task 9

    describe "Monad Writer" $ do
        let m1  = Writer' (Identity 1, "Number 1")
        let m2  = Writer' (Identity 5, "Number 5")
        let m3 = Writer' (Identity "Late", "Simple string")
        let m4 = Writer' (Identity (+1),  "Add 1")
        let m5 = Writer' (Identity (*100), "Mult 100")
        let m6 = Writer' (Identity head, "Head")

        it "Functor `Writer'`" $ do
            fmap (*3) m1 `shouldBe` (Writer' (Identity 3,  "Number 1")  :: Writer' String Int)
            fmap (>1) m2 `shouldBe` (Writer' (Identity True,  "Number 5") :: Writer' String Bool)
            fmap (++ "Very ") m3 `shouldBe` (Writer' (Identity "LateVery ",  "Simple string") :: Writer' String String)

        it "Applicative `Writer'`" $ do
            m4 <*> m1 `shouldBe` (Writer' (Identity 2,  "Add 1Number 1") :: Writer' String Int)
            m6 <*> m3 `shouldBe` (Writer' (Identity 'L',  "HeadSimple string") :: Writer' String Char)
            m5 <*> m2 `shouldBe` (Writer' (Identity 500,  "Mult 100Number 5") :: Writer' String Int)
