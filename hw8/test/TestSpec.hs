module TestSpec (spec) where

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader()
import           Data.Functor.Identity
import           Data.Map.Strict ()


import           Test.Hspec      (Spec, describe, it, shouldBe)

import           MyLib8       
import           MyLib9

spec :: Spec
spec = do
    describe "Task 1" $ do
        it "fromDo11" $ do
            let input1 = Just 5
                input2 = Just "test"
                result = fromDo11 input1 input2
            result `shouldBe` Just (15, "testabcd")
    
    describe "Task 2" $ do
        it "Pythagorean triples" $ do
            pythagoreanTriples 4 `shouldBe` ([])
            pythagoreanTriples 5 `shouldBe`  [(5, 4, 3)]
            pythagoreanTriples 10 `shouldBe` [(5, 4, 3), (10, 8, 6)]

    describe "Task 3a" $ do
        it "Functor" $ do
            let writerValue = Writer' (Identity 5, "log")
                mappedValue = fmap (* 2) writerValue
            runWriter' mappedValue `shouldBe` (Identity (10 :: Int), "log")

        it "Applicative" $ do
            let writerFunc = Writer' (Identity (* 2), "log1")
                writerValue = Writer' (Identity 5, "log2")
                appliedValue = writerFunc <*> writerValue
            runWriter' appliedValue `shouldBe` (Identity (10 :: Int), "log1log2")


        it "Monad" $ do
            let writerValue = Writer' (Identity 5, "log1")
                binderFunc x = Writer' (Identity (x * 2), "log2")
                boundValue = writerValue >>= binderFunc
            runWriter' boundValue `shouldBe` (Identity (10 :: Int), "log1log2")

    describe "Task 3b" $ do
        it "tell" $ do
            let writerValue = tell "log message"
            runWriter' writerValue `shouldBe` (Identity (), "log message")

        it "listen" $ do
            let writerValue = do
                    tell "log message"
                    return 5
            runWriter' (listen writerValue) `shouldBe` (Identity ((5 :: Int), "log message"), "log message")




        
    

        