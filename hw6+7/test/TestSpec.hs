module TestSpec (spec) where

import           Data.Char       (isDigit)
import           Data.Map.Strict (fromList)


-- import           Test.Hspec      (Spec, describe, it, shouldBe, shouldReturn)

import           HW6
import           HW7
import           Parser

spec :: Spec
spec = do

-- Task 1
--
  describe "simple parsers from task1" $ do
    it "newLineP_1" $ do
        runParser newLineP "" `shouldBe` Nothing
    it "newLineP_2" $ do
        runParser newLineP "\nabc" `shouldBe` Just ('\n', "abc")
    it "newLineP_3" $ do
        runParser newLineP "\n" `shouldBe` Just ('\n', "")