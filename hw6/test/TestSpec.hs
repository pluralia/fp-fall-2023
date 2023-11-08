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
  describe "test" $ do
    it "returns the correct result" $ do
        1 + 2 `shouldBe` 3
      
