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
