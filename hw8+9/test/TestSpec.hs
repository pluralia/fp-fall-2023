module TestSpec where

import MyLib
import Test.Hspec
import Data.Functor.Identity
import Data.Monoid (Sum(..))

spec :: Spec
spec = do    
    -- describe "6. Do-нотация" $ do
    --     it "fromDo11'" $
    --         fromDo11' (Just 1) (Just "ttt") `shouldBe` Just (11, "tttabcd")

    describe "7. Пифагоровы тройки" $ do
        it "pythagor" $ do
            pythagor 0 `shouldBe` ([] :: [(Int, Int, Int)])
            pythagor 1 `shouldBe` [(3, 4, 5) :: (Int, Int, Int)]
            pythagor 3 `shouldBe` [(3, 4, 5), (6, 8, 10), (5, 12, 13) :: (Int, Int, Int)]
            pythagor 5 `shouldBe` [(3,4,5), (6,8,10), (5,12,13), (9,12,15), (8,15,17) :: (Int, Int, Int)]
    
    describe "9. Monad `Writer`" $ do
        it "Monad" $ do
            let writer1 = (Writer' (Identity 5, "Log") :: Writer' String Int)
            let result1 = fmap (+ 1) writer1
            runWriter' result1 `shouldBe` (Identity 6, "Log")

            let result2 = pure 10 :: Writer' String Int
            runWriter' result2 `shouldBe` (Identity 10, "")

            let writer2 = Writer' (Identity (+ 5), "Add 5 ")
            let writer3 = (Writer' (Identity 7, "Number 7") :: Writer' String Int)
            let result3 = writer2 <*> writer3
            runWriter' result3 `shouldBe` (Identity 12, "Add 5 Number 7")

        it "sumAndTraceInOrder 1" $
            let (result, logRes) = runWriter' (sumAndTraceInOrder Leaf)
            in do
                runIdentity result `shouldBe` []
                logRes `shouldBe` (mempty :: Sum Int)

        it "sumAndTraceInOrder 2" $
            let tree = Node 5 Leaf Leaf
                (result, logRes) = runWriter' (sumAndTraceInOrder tree)
            in do
                runIdentity result `shouldBe` [5 :: Int]
                logRes `shouldBe` Sum 5

        it "sumAndTraceInOrder 3" $
            let tree = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)
                (result, logRes) = runWriter' (sumAndTraceInOrder tree)
            in do
                runIdentity result `shouldBe` [2, 1, 3 :: Int]
                logRes `shouldBe` Sum 6

        it "sumAndTraceInOrder 4" $
            let tree = Node 1
                            (Node 2
                                (Node 3 Leaf Leaf)
                                (Node 4 Leaf Leaf))
                            (Node 5
                                Leaf
                                (Node 6
                                    (Node 7 Leaf Leaf)
                                    Leaf))
                expectedLogSum = Sum $ sum [1..7] :: Sum Int
                (result, logRes) = runWriter' (sumAndTraceInOrder tree)
            in do
                runIdentity result `shouldSatisfy` (\res -> length res == 7)
                logRes `shouldBe` expectedLogSum

    describe "10. Monad `Reader`" $ do
        it "Reader'" $ do
            testEvalExpr `shouldBe` (Just 5 :: Maybe Int)
            testEvalStmts `shouldBe` (Just 9 :: Maybe Int)