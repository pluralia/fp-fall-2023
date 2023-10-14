module TestSpec (spec) where
    
import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

import MyLib


spec :: Spec
spec = do
  describe "traceFoldl" $ do
    it "returns the correct result" $ do
      traceFoldl (+) 0 [1, 2, 3] `shouldBe` (6 :: Int)
      traceFoldl (*) 1 [1, 2, 3, 4] `shouldBe` (24 :: Int)
