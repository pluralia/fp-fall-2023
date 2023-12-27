module TestSpec (spec) where

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader()
import           Data.Functor.Identity
import           Data.Map.Strict ()
import           Data.Monoid (Sum(..))
import           Control.Monad.State.Lazy
import qualified System.Random as R


import           Test.Hspec      (Spec, describe, it, shouldBe,shouldSatisfy)

import           MyLib8       
import           MyLib9

spec :: Spec
spec = do
    describe "Task 1" $ do
        it "fromDo11" $ do
            fromDo11 (Just 5 :: Maybe Int) (Just "test" :: Maybe String) `shouldBe` Just (15 :: Int, "testabcd" :: String) 
            fromDo11 (Nothing :: Maybe Int) (Just "test" :: Maybe String) `shouldBe` Nothing  
            fromDo11 (Just 5 :: Maybe Int) (Nothing :: Maybe String)      `shouldBe` Nothing  
            fromDo11 (Nothing :: Maybe Int) (Nothing :: Maybe String)      `shouldBe` Nothing 
    
    describe "Task 2" $ do
        it "Pythagorean triples" $ do
            pythagoreanTriples 4 `shouldBe` []
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
            runWriter' (listen writerValue) `shouldBe` (Identity (5 :: Int, "log message"), "log message")
    
    describe "Task 3c" $ do
        it "sumAndTraceInOrder 4" $ do
            let tree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf))
                expectedSum = 15
                (result, actualSum) = runWriter' (sumAndTraceInOrder tree)
            runIdentity result `shouldBe` ([2, 1, 4, 3, 5] :: [Int])
            getSum actualSum `shouldBe` expectedSum
            
    describe "Task 4" $ do
        it "testEvalExpr & testEvalStmts" $ do
            testEvalExpr  `shouldBe` (Just 5 :: Maybe Int)
            testEvalStmts `shouldBe` (Just 9 :: Maybe Int)
    
    describe "MyLib9" $ do
        it "getOne" $ do 
            let 
                bounds = (1, 100) :: (Int, Int)
                (value, _) = runState (getOne bounds) (R.mkStdGen 100 :: R.StdGen)
            value `shouldSatisfy` (\x -> x >= fst bounds && x <= snd bounds)





        
    

        