module TestSpec (spec) where

import Test.Hspec
import Control.Monad.State

import MyLib

spec :: Spec
spec = do
  describe "turnstile" $ do
    it "should work" $ do
      let 
        (r, _) = runState turnstile Locked
      r `shouldBe` Thank