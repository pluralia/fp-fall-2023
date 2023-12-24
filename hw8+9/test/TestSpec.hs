module TestSpec where

import MyLib
import Test.Hspec
import Data.Functor.Identity

spec :: Spec
spec = do    
    describe "7. Пифагоровы тройки" $ do
        it "pythagor" $ do
            pythagor 0 `shouldBe` ([] :: [(Int, Int, Int)])
            pythagor 1 `shouldBe` [(3, 4, 5) :: (Int, Int, Int)]
            pythagor 3 `shouldBe` [(3, 4, 5), (6, 8, 10), (5, 12, 13) :: (Int, Int, Int)]
            pythagor 5 `shouldBe` [(3,4,5), (6,8,10), (5,12,13), (9,12,15), (8,15,17) :: (Int, Int, Int)]
    
    describe "9. Monad `Writer`" $ do
        it "9.a Реализация`Monad`" $ do
            let writer1 = (Writer' (Identity 5, "Log") :: Writer' String Int)
            let result1 = fmap (+ 1) writer1
            runWriter' result1 `shouldBe` (Identity 6, "Log")

            let result2 = pure 10 :: Writer' String Int
            runWriter' result2 `shouldBe` (Identity 10, "")

            let writer2 = Writer' (Identity (+ 5), "Add 5 ")
            let writer3 = (Writer' (Identity 7, "Number 7") :: Writer' String Int)
            let result3 = writer2 <*> writer3
            runWriter' result3 `shouldBe` (Identity 12, "Add 5 Number 7")