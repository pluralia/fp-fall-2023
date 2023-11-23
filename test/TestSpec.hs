module TestSpec where

import Test.Hspec
import MyLib
import           Data.Functor.Identity
import           Data.Monoid (Sum(..))

k :: Int -> Writer' String Int
k x = Writer' (Identity x + 27, " стрелка Клейсли! ")

spec :: Spec
spec = do
    describe "6. Do-нотация" $ do
        it "fromDo12 and withoutDo12" $ do
            fromDo12 [1, 2, 3, 4, 5] (Just 't') `shouldBe` withoutDo12 [1, 2, 3, 4, 5] (Just 't')
            fromDo12 [5, 4, 3, 2, 1] (Just 'a') `shouldBe` withoutDo12 [5, 4, 3, 2, 1] (Just 'a')
            fromDo12 [0, 0]          (Just 'c') `shouldBe` withoutDo12 [0, 0]          (Just 'c')
            fromDo12 []              (Just 's') `shouldBe` withoutDo12 []              (Just 's')
    
    describe "7. Пифагоровы тройки" $ do
        it "test for different n" $ do
            pifagor 4 `shouldBe` ([] :: [(Int, Int, Int)])
            pifagor 5 `shouldBe` ([(5, 4, 3)] :: [(Int, Int, Int)])
            pifagor 10 `shouldBe` ([(5, 4, 3), (10, 8, 6)] :: [(Int, Int, Int)])
            pifagor 27 `shouldBe` ([(5,4,3), (10,8,6), (13,12,5), (15,12,9), (17,15,8), 
                                    (20,16,12), (25,20,15), (25,24,7), (26,24,10)] :: [(Int, Int, Int)])
    
    describe "9. Monad `Writer`" $ do

        let f1 = Writer' (Identity (*12),  "Function_1 ")
        let f2 = Writer' (Identity (+505), "Function_2 ")
        let x = Writer' (Identity 17, "x ")
        let y = Writer' (Identity 50, "y ")
        
        it "Functor `Writer'`" $ do
            fmap (*12) x `shouldBe` (Writer' (Identity 204,  "x ") :: Writer' String Int)
            fmap (+50) y `shouldBe` (Writer' (Identity 100,  "y ") :: Writer' String Int)
            fmap (>50) y `shouldBe` (Writer' (Identity False,  "y ") :: Writer' String Bool)
            fmap (<98) x `shouldBe` (Writer' (Identity True,  "x ") :: Writer' String Bool)
        it "Applicative `Writer'`" $ do 
            f1 <*> x `shouldBe` (Writer' (Identity 204,  "Function_1 x ") :: Writer' String Int)
            f1 <*> y `shouldBe` (Writer' (Identity 600,  "Function_1 y ") :: Writer' String Int)
            f2 <*> x `shouldBe` (Writer' (Identity 522,  "Function_2 x ") :: Writer' String Int)
            f2 <*> y `shouldBe` (Writer' (Identity 555,  "Function_2 y ") :: Writer' String Int)
        it "Monad `Writer'`" $ do
            (x >>= k) `shouldBe` (Writer' (Identity 44,  " стрелка Клейсли! ") :: Writer' String Int)            
            (y >>= k) `shouldBe` (Writer' (Identity 77,  " стрелка Клейсли! ") :: Writer' String Int)

        let myTree1 = Node 5 (Node 4 (Node 3 (Node 7 (Node 8 Leaf Leaf) Leaf) Leaf) (Node 2 Leaf Leaf)) 
                             (Node 1 (Node 0 Leaf Leaf) (Node 10 Leaf Leaf))
        let myTree2 = Node 200 (Node 505 Leaf Leaf) (Node 72 Leaf Leaf)
        let myTree3 = Node 70 Leaf Leaf
        let myTree4 = Leaf

        it "BinaryTree" $ do
            sumAndTraceInOrder myTree1 `shouldBe` 
                (Writer' (Identity [5,4,3,7,8,2,1,0,10], Sum 40) :: Writer' (Sum Int) [Int])
            sumAndTraceInOrder myTree2 `shouldBe` 
                (Writer' (Identity [200, 505, 72], Sum 777) :: Writer' (Sum Int) [Int])
            sumAndTraceInOrder myTree3 `shouldBe` 
                (Writer' (Identity [70], Sum 70) :: Writer' (Sum Int) [Int])
            sumAndTraceInOrder myTree4 `shouldBe` 
                (Writer' (Identity [], Sum 0) :: Writer' (Sum Int) [Int])
            
    describe "10. Monad `Reader`" $ do
        it "Last task `Reader'`" $ do
            testEvalExpr `shouldBe` (Just 5 :: Maybe Int)
            testEvalStmts `shouldBe` (Just 9 :: Maybe Int)
