module TestSpec (spec) where

import           Data.Char       (isDigit)
import           Data.Map.Strict (fromList)


import           Test.Hspec      (Spec, describe, it, shouldBe, shouldReturn)

import           MyLib6        
import           MyLib7
import           Parser

spec :: Spec
spec = do

-- Task 1
--
  describe "simple parsers from task1" $ do
    it "newLineP" $ do
      runParser newLineP "" `shouldBe` Nothing
      runParser newLineP "Hello,World!" `shouldBe` Nothing
      runParser newLineP "\nHello,World!" `shouldBe` Just ('\n', "Hello,World!")

    it "intP" $ do
      runParser intP "" `shouldBe` Nothing
      runParser intP "Hello,World!" `shouldBe` Nothing
      runParser intP "20yearsold" `shouldBe` Just (20, "yearsold")

    it "floatP" $ do
      runParser floatP "" `shouldBe` Nothing
      runParser floatP "Hi" `shouldBe` Nothing
      runParser floatP "1 litre" `shouldBe` Nothing
      runParser floatP "1.5 litre" `shouldBe` Just (1.5 :: Float, " litre")


    it "stringP" $ do
      runParser (stringP "hello") "world" `shouldBe` Nothing
      runParser (stringP "abc") "abcdefg" `shouldBe` Just ("abc", "defg")


-- -- Task 2
--
    describe "task 2" $ do
      it "multIntsP" $ do
        runParser multIntsP "1*2"  `shouldBe` Just (2 :: Int, "")
        runParser multIntsP "1*"  `shouldBe` Nothing
        runParser multIntsP "*"  `shouldBe` Nothing
        runParser multIntsP "*1"  `shouldBe` Nothing
        runParser multIntsP "12*2"  `shouldBe` Just (24 :: Int, "")
        runParser multIntsP "12 * 2"  `shouldBe` Just (24 :: Int, "")