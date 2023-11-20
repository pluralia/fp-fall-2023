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
