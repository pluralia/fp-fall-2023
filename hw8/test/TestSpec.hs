module TestSpec (spec) where

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict as M


import           Test.Hspec      (Spec, describe, it, shouldBe, shouldReturn)

import           MyLib8       
import           MyLib9

spec :: Spec
spec = do
    describe "Task 1" $ do
        it "fromDo11" $ do
            let input1 = Just 5
                input2 = Just "test"
                result = fromDo11 input1 input2
            result `shouldBe` Just (15, "testabcd")