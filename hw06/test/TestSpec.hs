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
  describe "newLineP" $ do
    it "Single newline" $
      runParser newLineP "\n" `shouldBe` Just ('\n', "")
    it "One character" $
      runParser newLineP "a" `shouldBe` Nothing
    it "Parses a newline character among other characters" $
      runParser newLineP "abc\n123" `shouldBe` Nothing
    it "Parses normal new string" $
      runParser newLineP "\nabc . asd" `shouldBe` Just ('\n', "abc . asd")
