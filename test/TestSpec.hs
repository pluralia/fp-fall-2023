module TestSpec (spec) where

import MyLib
import Parser

import Data.Char           (isDigit)
import Data.Map.Strict     (fromList) -- cabal поругался, но когда последний тест заработает, импорт понадобится

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  )

spec :: Spec
spec = do 
    it "Task 1. Simple Parsers" $ do
      runParser newLineP "\nHello, world!" `shouldBe` Just ('\n', "Hello, world!")
      runParser newLineP "Hello, \nworld!" `shouldBe` Nothing
      runParser newLineP "\n567"           `shouldBe` Just ('\n',"567")

      runParser intP "1234" `shouldBe` (Just (1234, "")  :: Maybe (Int, String))
      runParser intP "h5 " `shouldBe` (Nothing         :: Maybe (Int, String))
      runParser intP "012."  `shouldBe` (Just (12, ".")  :: Maybe (Int, String))
      runParser intP "0"     `shouldBe` (Just (0, "")    :: Maybe (Int, String))

      runParser dnaSeqP "GATTACA" `shouldBe` (Just ([G,A,T,T,A,C,A],"") :: Maybe ([DNA], String))
      runParser dnaSeqP "gattaca" `shouldBe` (Just ([], "gattaca")      :: Maybe ([DNA], String))
      runParser dnaSeqP "GART"    `shouldBe` (Just ([G,A],"RT")         :: Maybe ([DNA], String))
      runParser dnaSeqP "RTGT"    `shouldBe` (Just ([],"RTGT")          :: Maybe ([DNA], String))

    it "Task 2. Combinated parsers" $ do
      runParser multIntsP "33 * 6"    `shouldBe` (Just (198, "")  :: Maybe (Int, String))
      runParser multIntsP "0*72 lk"   `shouldBe` (Just (0, " lk") :: Maybe (Int, String))
      runParser multIntsP "b 30 * 71" `shouldBe` (Nothing         :: Maybe (Int, String))

      runParser floatP "36,6" `shouldBe` (Just (36.6, "") :: Maybe (Float, String))
      runParser floatP "36.6" `shouldBe` (Nothing         :: Maybe (Float, String))

      runParser multFloatsP "36,6 * 2,7"    `shouldBe` (Just (98.82,"")     :: Maybe (Float, String))
      runParser multFloatsP "36.6 * 2,7"    `shouldBe` (Nothing             :: Maybe (Float, String))
      runParser multFloatsP "36,6 * 2,7boo" `shouldBe` (Just (98.82,"boo")  :: Maybe (Float, String))

      runParser simpleExprP "3,14 + 36,6"  `shouldBe` (Just (SimpleExpr 3.14 '+' 36.6,"") :: Maybe (SimpleExpr, String))
      runParser simpleExprP "3,14 * 36,6"  `shouldBe` (Just (SimpleExpr 3.14 '*' 36.6,"") :: Maybe (SimpleExpr, String))
      runParser simpleExprP ""             `shouldBe` Nothing

      (\(x, s) -> (abs (x - 39.74) < 1e-5, s)) <$> runParser sumMultFloatsP "3,14 + 36,6" `shouldBe` (Just (True, "")   :: Maybe (Bool, String))
      (\(x, s) -> (abs (x - 114.924) < 1e-5, s)) <$> runParser sumMultFloatsP "3,14 * 36,6" `shouldBe` (Just (True, "")   :: Maybe (Bool, String))

    it "Task 4. Difficult Parcers" $ do
      runParser (takeWhileP isDigit) "123JMK" `shouldBe` (Just ("123","JMK") :: Maybe (String, String))
      runParser (takeWhileP isDigit) "JMK123" `shouldBe` (Just ("","JMK123") :: Maybe (String, String))

      runParser eofP ""      `shouldBe` (Just ((), "") :: Maybe ((), String))
      runParser eofP "world" `shouldBe` (Nothing       :: Maybe ((), String))

    it "Task 5. Value Parser" $ do
      runParser valueP "123"   `shouldBe` (Just (IntValue 123, "")       :: Maybe (Value, String))
      runParser valueP "36,6"  `shouldBe` (Just (FloatValue 36.6, "")    :: Maybe (Value, String))
      runParser valueP ""      `shouldBe` (Nothing                       :: Maybe (Value, String))
      runParser valueP "value" `shouldBe` (Just (StringValue "value","") :: Maybe (Value, String))

    it "Task 6. CSV" $ do
      runParser (abstractRowP ',' intP) "1, 2, 3, end" `shouldBe` (Just ([1,2,3],", end") :: Maybe ([Int], String))
      runParser (abstractRowP ':' symbolsP) "I:5:like:7:haskell!" `shouldBe` (Just (["I","5","like","7","haskell"],"!") :: Maybe ([String], String))


      let colName = ["Name", "Surname", "Course", "Grade"]
      runParser (rowP colName ',') "Julia, Kostina, Haskell, 5,5" `shouldBe` (Just (Row (fromList [("Course",StringValue "Haskell"),
                                                                                                   ("Grade",FloatValue 5.5),
                                                                                                   ("Name",StringValue "Julia"),
                                                                                                   ("Surname",StringValue "Kostina")]),"") :: Maybe (Row, String))
