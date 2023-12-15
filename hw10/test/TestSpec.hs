module TestSpec where

import Test.Hspec
import Control.Monad.State
import MyLib

spec :: Spec
spec = do
    describe "turnstile" $ do
        it "returns [Thank, Thank, Open, Tut] when the input is [Coin, Coin, Push, Push]" $ do
            evalState (turnstile [Coin, Coin, Push, Push]) Locked `shouldBe` [Thank, Thank, Open, Tut]

        it "returns [Thank, Open, Tut] when the input is [Coin, Push, Push]" $ do
            evalState (turnstile [Coin, Push, Push]) Locked `shouldBe` [Thank, Open, Tut]

        it "returns [Thank, Thank, Open, Thank, Open, Tut] when the input is [Coin, Coin, Push, Coin, Push, Push]" $ do
            evalState (turnstile [Coin, Coin, Push, Coin, Push, Push]) Locked `shouldBe` [Thank, Thank, Open, Thank, Open, Tut]