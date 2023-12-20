module TestSpec (spec) where

import           Data.Char       (isDigit)
import           Data.Map.Strict (fromList)


import           Test.Hspec      (Spec, describe, it, shouldBe, shouldReturn)

import           HW6
import           HW7
import           Parser

spec :: Spec
spec = do

-- Task 1
--
  describe "simple parsers from task1" $ do
    it "newLineP" $ do
      runParser newLineP "" `shouldBe` Nothing
      runParser newLineP "abc" `shouldBe` Nothing