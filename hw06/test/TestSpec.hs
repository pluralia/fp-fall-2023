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
