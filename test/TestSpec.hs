module TestSpec where

import Test.Hspec
import MyLib ()

spec :: Spec
spec = do
    describe "" $ do
        it "" $ do
            True `shouldBe` True
