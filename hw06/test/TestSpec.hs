module TestSpec where

import MyLib
import Parser
import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )
import Data.Char (isDigit, isAsciiLower)

spec :: Spec
spec = do
  describe "Simple parsers" $ do
    it "newLineP" $ do
      runParser newLineP "\n" `shouldBe` Just ('\n', "")
      runParser newLineP "a" `shouldBe` Nothing
      runParser newLineP "abc\n123" `shouldBe` Nothing
      runParser newLineP "\nabc . asd" `shouldBe` Just ('\n', "abc . asd")
    it "intP" $ do
      runParser intP "12312ppo" `shouldBe` Just (12312, "ppo")
      runParser intP "trtrtr" `shouldBe` Nothing
      runParser intP "-123abc" `shouldBe` Just (-123, "abc")
    it "stringP" $ do
      runParser (stringP "") "hello" `shouldBe` Just ("", "hello")
      runParser (stringP "hello") "hello world" `shouldBe` Just ("hello", " world")
      runParser (stringP "world") "hello" `shouldBe` Nothing
      runParser (stringP "hello") "hell" `shouldBe` Nothing
    it "floatP" $ do
      runParser floatP "" `shouldBe` Nothing
      runParser floatP "text" `shouldBe` Nothing
      runParser floatP "1234text" `shouldBe` Nothing
      runParser floatP "-222.333text" `shouldBe` Just (-222.333 :: Float, "text")
      runParser floatP "222.333text" `shouldBe` Just (222.333 :: Float, "text")
    it "valueP" $ do
      runParser valueP "" `shouldBe` Nothing
      runParser valueP "222text" `shouldBe` Just (IntValue (222 :: Int), "text")
      runParser valueP "-222.333text" `shouldBe` Just (FloatValue (-222.333 :: Float), "text")
      runParser valueP "222.333text" `shouldBe` Just (FloatValue (222.333 :: Float), "text")
      runParser valueP "text" `shouldBe` Just (StringValue ("text" :: String), "")
      runParser valueP "text1, text2" `shouldBe` Just (StringValue ("text1" :: String), ", text2")

  describe "Parser combinators and arithmetic" $ do
    it "multIntsP" $ do
      runParser multIntsP "3*6text" `shouldBe` Just (18:: Int, "text")
      runParser multIntsP "-3*6" `shouldBe` Just (-18:: Int, "")
      runParser multIntsP "text" `shouldBe` Nothing
    it "multFloatsP" $ do
      runParser multFloatsP "3.9*6.1text" `shouldBe` Just (23.79 :: Float, "text")
      runParser multFloatsP "-3.9*6.1" `shouldBe` Just (-23.79 :: Float, "")
      runParser multFloatsP "text" `shouldBe` Nothing
    it "simpleExprP" $ do
      runParser simpleExprP "-3.1*4.5"  `shouldBe` Just (SimpleExpr (-3.1) '*' 4.5, "")
      runParser simpleExprP "-33.1+56.0text"  `shouldBe` Just (SimpleExpr (-33.1) '+' 56.0,"text")
      runParser simpleExprP "-33.1+560text"  `shouldBe` Nothing
    it "exprP" $ do
      runParser exprP "-3.1*4.5"  `shouldBe` Just (Expr (-3.1) Mult 4.5, "")
      runParser exprP "-33.1+56.0text"  `shouldBe` Just (Expr (-33.1) Sum 56.0,"text")
      runParser exprP "1.11*"  `shouldBe` Nothing
      runParser exprP "-33.1+560text"  `shouldBe` Nothing
    it "sumMultFloatsP" $ do
      runParser sumMultFloatsP "-3.1*4.5"  `shouldBe` Just (-13.95, "")
      runParser sumMultFloatsP "-33.1+56.3text"  `shouldBe` Just (23.2, "text")
      runParser sumMultFloatsP "1.11*"  `shouldBe` Nothing
      runParser sumMultFloatsP "-33.1+560text"  `shouldBe` Nothing

  describe "More complex parsers" $ do
    it "takeWhileP" $ do
      let parser1 = takeWhileP isAsciiLower
      runParser parser1 "helloWorld" `shouldBe` Just ("hello", "World")
      let parser2 = takeWhileP isDigit
      runParser parser2 "123abc" `shouldBe` Just ("123", "abc")
      let parser3 = takeWhileP (== ' ')
      runParser parser3 "    Hello" `shouldBe` Just ("    ", "Hello")
      let parser4 = takeWhileP ( == ' ')
      runParser parser4 "Hello" `shouldBe` Just ("", "Hello")
    it "eofP" $ do
      runParser eofP "" `shouldBe` Just ((), "")
      runParser eofP "Hello" `shouldBe` Nothing
    it "inBetweenP" $ do
      runParser (inBetweenP "!" "," intP) "!12312," `shouldBe` Just (12312, "")
      runParser (inBetweenP "!" "," intP) ",12312!" `shouldBe` Nothing
    it "listP" $ do
      runParser (listP floatP) "[1.5, 2.3, 3.7] abc" `shouldBe` Just ([1.5, 2.3, 3.7] :: [Float], " abc") 
      runParser (listP intP) "[]" `shouldBe` Just ([] :: [Int], "")
      runParser (listP symbolsP) "[abc, def, ghi]" `shouldBe` Just (["abc", "def", "ghi"] :: [String], "")
      runParser (listP intP) "1, 2, 3]" `shouldBe` Nothing