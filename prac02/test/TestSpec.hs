module TestSpec (spec) where

import MyLib (
    myInit
  )

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

spec :: Spec
spec = do
  describe "test `myInit`" $ do
    it "empty list" $ do
      myInit [] `shouldBe` (Nothing :: Maybe [Int])
  
    it "1 item" $ do
      myInit [1] `shouldBe` (Just [] :: Maybe [Int])

    it "many items" $ do
      myInit [1..10] `shouldBe` (Just [1..9] :: Maybe [Int])
