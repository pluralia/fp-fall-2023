module TestSpec where

import Test.Hspec
import MyLib

spec :: Spec
spec = do
    True `shouldBe` True