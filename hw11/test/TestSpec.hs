module TestSpec (spec) where

import Test.Hspec
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified System.Random as R

import MyLib

spec :: Spec
spec = do
  describe "logMsg" $ do
    it "should add a message to the log" $ do
      let 
        (_, logs) = runWriter $ logMsg "Test message"
      logs `shouldBe` [Log 1 "Test message"]
