module TestSpec (spec) where

import Test.Hspec
import Control.Monad.State
import MyLib

spec :: Spec
spec = do
  describe "test" $ do
    it "test" $ do
      1 + 1 `shouldBe` 2