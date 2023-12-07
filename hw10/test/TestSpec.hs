module TestSpec (spec) where

import           MyLib


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
      it "turnstile tests" $ do
        let 
          result1 = exampleResult [Coin, Coin, Push, Push, Coin]
          result2 = exampleResult [Coin, Push, Push, Coin, Coin, Push]
          result3 = exampleResult [Push, Push, Coin, Push]
        
        result1 `shouldBe` [Thank, Thank, Open, Tut, Thank]
        result2 `shouldBe` [Thank, Open, Tut, Thank, Thank, Open]
        result3 `shouldBe` [Tut, Tut, Thank, Open]

        