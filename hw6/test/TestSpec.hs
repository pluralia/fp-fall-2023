module TestSpec (spec) where
    
import Data.Map.Strict     (fromList)
import Data.Char           (isDigit)


import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

import MyLib
import Parser

spec :: Spec
spec = do
  describe "simple parsers" $ do
    it "newLineP" $ do
      runParser newLineP "\n" `shouldBe` Just ('\n', "")
      runParser newLineP "a" `shouldBe` Nothing
    
    it "intP" $ do
      runParser intP "123abc" `shouldBe` Just (123 :: Int, "abc")
      runParser intP "abc" `shouldBe` Nothing

    it "dnaP" $ do
      runParser dnaP "ATGCTGAC" `shouldBe` Just (A :: DNA, "TGCTGAC")
      runParser dnaP "Cabc" `shouldBe` Just (C :: DNA, "abc")
      runParser dnaP "abc" `shouldBe` Nothing

    it "stringP" $ do
      runParser (stringP "abc") "abcdef" `shouldBe` Just ("abc" :: String, "def")
      runParser (stringP "abc") "defabc" `shouldBe` Nothing
      runParser (stringP "abc") "ab" `shouldBe` Nothing

  describe "multDigitsP" $ do
    it "works" $ do
      runParser multDigitsP "9 * 7 ABC" `shouldBe` Just (63 :: Int, " ABC")
      runParser multDigitsP "9 + 7" `shouldBe` Nothing
      runParser multDigitsP "99 * 7" `shouldBe` Nothing

  describe "multIntsP" $ do
    it "works" $ do
      runParser multIntsP "9 * 7 ABC" `shouldBe` Just (63 :: Int, " ABC")
      runParser multIntsP "9 + 7" `shouldBe` Nothing
      runParser multIntsP "99 * 7" `shouldBe` Just (693 :: Int, "")

  describe "multFloatsP" $ do
    it "works" $ do
      runParser multFloatsP "123.1 * 6.9 abc" `shouldBe` Just (849.39 :: Float, " abc")
      runParser multFloatsP "123 * 6 abc" `shouldBe` Nothing
      runParser multFloatsP "123.4 + 6.7 abc" `shouldBe` Nothing

  describe "sumMultFloatsP" $ do
    it "works" $ do
      runParser sumMultFloatsP "123.1 * 6.9 abc" `shouldBe` Just (849.39 :: Float, " abc")
      runParser sumMultFloatsP "12.3 + 6.1 abc" `shouldBe` Just (18.4 :: Float, " abc")
      runParser sumMultFloatsP "123.4 + 6.7 abc" `shouldBe` Just (130.1 :: Float, " abc")
      runParser sumMultFloatsP "123.4 / 6.7 abc" `shouldBe` Nothing

  describe "takeWhileP" $ do
    it "works" $ do
      runParser (takeWhileP isDigit) "123 abc" `shouldBe` Just ("123" :: String, " abc")
      runParser (takeWhileP isDigit) "abc" `shouldBe` Just ("" :: String, "abc")

  describe "eofP" $ do
    it "works" $ do
      runParser eofP "123 abc" `shouldBe` Nothing
      runParser eofP "" `shouldBe` Just ((), "")

  describe "inBetweenP" $ do
    it "works" $ do
      runParser (inBetweenP "(" ")" (takeWhileP isDigit)) "(123)" `shouldBe` Just ("123" :: String, "")
      runParser (inBetweenP "(" ")" (takeWhileP isDigit)) "(123" `shouldBe` Nothing
      runParser (inBetweenP "(" ")" (takeWhileP isDigit)) "123)" `shouldBe` Nothing
      runParser (inBetweenP "[" "]" (takeWhileP isDigit)) "[123] abc" `shouldBe` Just ("123" :: String, " abc")

  describe "listP" $ do
    it "works" $ do
      runParser (listP intP) "[1, 2, 3] abc" `shouldBe` Just ([1, 2, 3] :: [Int], " abc") 
      runParser (listP intP) "[1, 2, 3" `shouldBe` Nothing
      runParser (listP multDigitsP) "[1 * 2, 3 * 4, 5 * 6] abc" `shouldBe` Just ([2, 12, 30] :: [Int], " abc")

  describe "valueP" $ do
    it "works" $ do
      runParser valueP "123 abc" `shouldBe` Just (IntValue 123 :: Value, " abc")
      runParser valueP "123.4 abc" `shouldBe` Just (FloatValue 123.4 :: Value, " abc")
      runParser valueP "abc" `shouldBe` Just (StringValue "abc" :: Value, "")

  describe "rowP" $ do
    it "works" $ do
      runParser (rowP ["a", "b", "c"]) "1,2,3" `shouldBe` Just (Row (fromList [("a" :: String, IntValue 1), ("b" :: String, IntValue 2), ("c" :: String, IntValue 3)]), "")
      runParser (rowP ["a", "b", "c"]) "a,b,c" `shouldBe` Just (Row (fromList [("a" :: String, StringValue "a"), ("b" :: String, StringValue "b"), ("c" :: String, StringValue "c")]), "")
      runParser (rowP ["a", "b", "c"]) "1.2,2.3,3.4" `shouldBe` Just (Row (fromList [("a" :: String, FloatValue 1.2), ("b" :: String, FloatValue 2.3), ("c" :: String, FloatValue 3.4)]), "")