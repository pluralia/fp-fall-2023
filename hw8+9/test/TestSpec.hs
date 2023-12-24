module TestSpec where

import MyLib
import Test.Hspec

spec :: Spec
spec = do    
    describe "7. Пифагоровы тройки" $ do
        it "test for different n" $ do
            pythagor 4 `shouldBe` ([] :: [(Int, Int, Int)])
            pythagor 5 `shouldBe` [(3, 4, 5) :: (Int, Int, Int)]
            pythagor 13 `shouldBe` [(3, 4, 5), (6, 8, 10), (5, 12, 13) :: (Int, Int, Int)]
            pythagor 17 `shouldBe` [(3,4,5), (6,8,10), (5,12,13), (9,12,15), (8,15,17) :: (Int, Int, Int)]