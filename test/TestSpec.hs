module TestSpec where

import Test.Hspec
import MyLib
import Control.Monad.State

spec :: Spec
spec = do
    describe "1. State" $ do
        it "different inputs tests" $ do
            let input1 = [Push, Coin, Push, Push, Coin]                         :: [TurnstileInput]
                input2 = [Coin, Coin, Coin, Push, Push]                         :: [TurnstileInput]
                input3 = [Push, Coin, Coin, Push, Push, Coin, Push, Push, Coin] :: [TurnstileInput]

            runState (turnstile input1) Locked `shouldBe` ([Tut,Thank,Open,Tut,Thank], Unlocked)
            runState (turnstile input2) Locked `shouldBe` ([Thank,Thank,Thank,Open,Tut], Locked)
            runState (turnstile input3) Locked `shouldBe` ([Tut,Thank,Thank,Open,Tut,Thank,Open,Tut,Thank], Unlocked)
