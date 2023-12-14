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
    describe "task 2" $ do
      it "multIntsP" $ do
        runParser multIntsP "1*2"  `shouldBe` Just (2 :: Int, "")
        runParser multIntsP "1*"  `shouldBe` Nothing
        runParser multIntsP "*"  `shouldBe` Nothing
        runParser multIntsP "*1"  `shouldBe` Nothing
        runParser multIntsP "12*2"  `shouldBe` Just (24 :: Int, "")
        runParser multIntsP "12 * 2"  `shouldBe` Just (24 :: Int, "")
        runParser multFloatsP "1.0 * 3.0" `shouldBe` Just (3.0 :: Float, "")
        runParser multFloatsP "0.0 * 3.0" `shouldBe` Just (0.0 :: Float, "")
        runParser multFloatsP "*" `shouldBe` Nothing
        runParser sumMultFloatsP "3.0+1.4"  `shouldBe` Just (4.4 :: Float, "")
        runParser sumMultFloatsP "3.0*1.4"  `shouldBe` Just (4.2 :: Float, "")
        runParser sumMultFloatsP "*"  `shouldBe` Nothing

-- -- -- Task 4
    describe "task4" $ do
      it "takeWhileP" $ do
        runParser (takeWhileP (== '?')) "a" `shouldBe` Just ("", "a")
        runParser (takeWhileP (== '?')) "?" `shouldBe` Just ("?", "")
        runParser (takeWhileP isDigit) "123Hello" `shouldBe` Just ("123", "Hello")
      
      it "eofP" $ do
        runParser eofP "" `shouldBe` Just ((), "")
        runParser eofP "\n" `shouldBe` Nothing
        runParser eofP "123" `shouldBe` Nothing
      
      it "inBetweenP" $ do
        runParser (inBetweenP "(" ")" intP) "(812)" `shouldBe` Just (812 :: Int, "")
        runParser (inBetweenP "(" ")" digitP) "(812)" `shouldBe` Nothing

        
