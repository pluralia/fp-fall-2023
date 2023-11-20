{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module TestSpec where

import Test.Hspec
import MyLib


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
            transpose ([] :: [[Int]]) `shouldBe` ([]                                :: [[Int]])

        it "transposes a square matrix" $ do
            let matrix = ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]])
            transpose matrix          `shouldBe` ([[1, 4, 7], [2, 5, 8], [3, 6, 9]] :: [[Int]])

        it "transposes a rectangular matrix" $ do
            let matrix = ([[1, 2, 3], [4, 5, 6]] :: [[Int]])
            transpose matrix          `shouldBe` ([[1, 4], [2, 5], [3, 6]]          :: [[Int]])

    -- describe "9.c Реализуйте обход (любой) бинарного дерева и суммируйте элементы в вершинах с помощью Writer'" $ do
    --     it "returns an empty list and sum 0 for an empty tree" $
    --         runWriter' (sumAndTraceInOrder Leaf) `shouldBe` Writer' (Identity [], Sum (0 :: Int))

    --     it "sums elements in order for a simple tree" $
    --         runWriter' (sumAndTraceInOrder (Node 1 Leaf Leaf)) `shouldBe` Writer' (Identity [1], Sum (1 :: Int))

    --     it "sums elements in order for a more complex tree" $
    --         let tree = Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf (Node 9 Leaf Leaf))
    --         in runWriter' (sumAndTraceInOrder tree) `shouldBe` Writer' (Identity [3, 5, 7, 9], Sum (24 :: Int))