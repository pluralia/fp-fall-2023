module TestSpec (spec) where

import MyLib
import           Control.Monad.Writer.Strict
import           Data.Functor.Identity
import           Data.Monoid ()

import Test.Hspec
  (
    Spec
  , it
  , describe
  , shouldBe
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "fromDo11 tests" $ do
        myFromDo11 (Just 15 :: Maybe Int) (Just "apple" :: Maybe String) `shouldBe` Just (25 :: Int, "appleabcd" :: String) 
        myFromDo11 (Nothing :: Maybe Int) (Just "apple" :: Maybe String) `shouldBe` Nothing  
        myFromDo11 (Just 15 :: Maybe Int) (Nothing :: Maybe String)      `shouldBe` Nothing  
        myFromDo11 (Nothing :: Maybe Int) (Nothing :: Maybe String)      `shouldBe` Nothing 
        -- Оригинальная функция зависает на любом запросе:
        -- ghci> fromDo11 (Just 15 :: Maybe Int) (Just "apple" :: Maybe String)
        -- Just (

      it "fromDo12 tests" $ do
        fromDo12 ([9, 9, 9, 9, 9] :: [Int]) (Just 'q' :: Maybe Char) `shouldBe` myFromDo12 ([9, 9, 9, 9, 9] :: [Int]) (Just 'q' :: Maybe Char)
        fromDo12 ([9, 9, 9, 9, 9] :: [Int]) (Nothing :: Maybe Char)  `shouldBe` myFromDo12 ([9, 9, 9, 9, 9] :: [Int]) (Nothing :: Maybe Char)

      it "pythagoreanTriples tests" $ do
        take 6 pythagoreanTriples `shouldBe` ([(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)] :: [(Int, Int, Int)])

      it "ReturnableCalculation tests" $ do
        returnExample `shouldBe` ReturnableCalculation {runCalculation = Left (42 :: Int)}

      it "sumAndTraceInOrder tests" $ do
        let exampleTree = Node (1 :: Int) (Node (2 :: Int) Leaf Leaf) (Node (3 :: Int) (Node (4 :: Int) Leaf Leaf) (Node (5 :: Int) Leaf Leaf))
        sumAndTraceInOrder exampleTree `shouldBe` Writer' {runWriter' = (Identity ([2,1,4,3,5] :: [Int]), Sum {getSum = 15 :: Int})}

      it "ReturnableCalculation tests" $ do
        let 
          one :: Writer' String Int
          one = do
            tell "a"
            tell "p"
            tell "p"
            tell "l"
            tell "e"
            pure 15
        runWriter' one `shouldBe` ((Identity 15, "apple") :: (Identity Int, String))
        let 
          two :: Writer' String Int
          two = do
            tell "b"
            tell "l"
            tell "a"
            tell "c"
            tell "k"
            pure 999
        runWriter' two `shouldBe` ((Identity 999, "black") :: (Identity Int, String))

      it "testEvalExpr tests" $ do
        testEvalExpr  `shouldBe` (Just 5 :: Maybe Int)

      it "testEvalExpr tests" $ do
        testEvalStmts `shouldBe` (Just 9 :: Maybe Int)
