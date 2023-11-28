{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module TestSpec where

import Test.Hspec
import MyLib
import           Data.Functor.Identity
import           Data.Monoid (Sum(..))


spec :: Spec
spec = do
    describe "2. Реализуйте `rejectWithNegatives`" $ do
        it "returns Just [1, 2, 3] for [1, 2, 3]" $ do
            rejectWithNegatives ([1, 2, 3]  :: [Int])  `shouldBe` (Just [1, 2, 3] :: Maybe [Int])

        it "returns Nothing for [-1, 2, 3]" $ do
            rejectWithNegatives ([-1, 2, 3] :: [Int])  `shouldBe` (Nothing        :: Maybe [Int])

        it "returns Just [] for an empty list" $ do
            rejectWithNegatives ([]         :: [Int])  `shouldBe` (Just []        :: Maybe [Int])

            
    describe "3. Используйте Traversable для реализации транспонирования матриц" $ do
        it "transposes an empty matrix" $ do
            transpose ([[]] :: [[Int]]) `shouldBe` ([]                              :: [[Int]])

        it "transposes a square matrix" $ do
            let matrix = ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]])
            transpose matrix          `shouldBe` ([[1, 4, 7], [2, 5, 8], [3, 6, 9]] :: [[Int]])

        it "transposes a rectangular matrix" $ do
            let matrix = ([[1, 2, 3], [4, 5, 6]] :: [[Int]])
            transpose matrix          `shouldBe` ([[1, 4], [2, 5], [3, 6]]          :: [[Int]])


    describe "7. Пифагоровы тройки" $ do
        it "pythagoreanTriples with different n" $ do
            pythagoreanTriples 3 `shouldBe` ([]                                    :: [(Integer, Integer, Integer)])
            pythagoreanTriples 5 `shouldBe` ([(5, 4, 3)]                           :: [(Integer, Integer, Integer)])
            pythagoreanTriples 13 `shouldBe` ([(5, 4, 3), (10, 8, 6), (13, 12, 5)] :: [(Integer, Integer, Integer)])


    describe "8. Задайте тип данных (ReturnableCalculation a) и сделайте 'ReturnableCalculation' монадой" $ do
        it "RealReturn should return the provided value" $
            realReturn (42 :: Int) `shouldBe` RealReturn (42 :: Int)

        it "RealReturn should not affect subsequent computations" $
            let result = do
                    _ <- realReturn (10 :: Int)
                    _ <- realReturn (20 :: Int)
                    pure (30 :: Int)
            in result `shouldBe` RealReturn (30 :: Int)


    describe "9.c Реализуйте обход (любой) бинарного дерева и суммируйте элементы в вершинах с помощью Writer'" $ do
        it "returns an empty list and sum 0 for an empty tree" $
            sumAndTraceInOrder Leaf `shouldBe` (Writer' (Identity [], Sum 0) :: Writer' (Sum Int) [Int])

        it "sums elements in order for a simple tree" $
            sumAndTraceInOrder (Node 1 Leaf Leaf) `shouldBe` (Writer' (Identity [1], Sum 1) :: Writer' (Sum Int) [Int])

        it "sums elements in order for a more complex tree" $
            let tree = Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf (Node 9 Leaf Leaf))
            in sumAndTraceInOrder tree `shouldBe` (Writer' (Identity [3, 5, 7, 9], Sum 24) :: Writer' (Sum Int) [Int])


    describe "10. Monad `Reader`" $ do
        it "testEvalExpr and testEvalStmts" $ do
            testEvalExpr  `shouldBe` (Just 5 :: Maybe Int)
            testEvalStmts `shouldBe` (Just 9 :: Maybe Int)
