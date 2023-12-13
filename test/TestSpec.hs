module TestSpec where

import Test.Hspec
import MyLib ()

spec :: Spec
spec = do
    describe "State" $ do
        it "your test:" $ do
            runState turnstile Locked   `shouldBe` ([Thank, Thank, Open, Tut, Thank], Unlocked)
            runState turnstile Unlocked `shouldBe` ([Thank, Thank, Open, Tut, Thank], Unlocked)
        it "my test:" $ do
            let action2     = [Push, Push, Coin, Coin, Push, Push, Coin, Push, Push] :: [TurnstileInput]
            let turnstile2  = mapM (fsm switch) action2
            runState turnstile2 Locked `shouldBe` ([Tut, Tut, Thank, Thank, Open, Tut, Thank, Open, Tut], Locked)
            runState turnstile2 Locked `shouldBe` ([Open, Tut, Thank, Thank, Open, Tut, Thank, Open, Tut], Locked)
