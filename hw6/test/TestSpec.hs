module TestSpec (spec) where

import MyLib
import Parser

import Data.Char           (isDigit)
import Data.Map.Strict     (fromList)

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "newLineP tests" $ do 
        runParser newLineP "\n"      `shouldBe` Just ('\n',"")
        runParser newLineP "\nWorld" `shouldBe` Just ('\n',"World")
      
      it "intP tests" $ do
        runParser intP "ABC"      `shouldBe` Nothing
        runParser intP "123 ABC"  `shouldBe` Just (123," ABC")
        runParser intP "123ABC23" `shouldBe` Just (123,"ABC23")

      it "dnaP tests" $ do
        runParser dnaP "TACG ACC" `shouldBe` Just (T,"ACG ACC")
        runParser dnaP "CC"       `shouldBe` Just (C,"C")
        runParser dnaP ""         `shouldBe` Nothing

      it "dnaSeqP tests" $ do
        runParser dnaSeqP "TACGACC"  `shouldBe` Just ([T,A,C,G,A,C,C],"")
        runParser dnaSeqP "CC TT"    `shouldBe` Just ([C,C]," TT")
        runParser dnaSeqP ""         `shouldBe` Just ([],"")

      it "stringP tests" $ do
        runParser (stringP "Hello") "Hello, World!"            `shouldBe` Just ("Hello",", World!")
        runParser (stringP "Hello, World!") "Hello, World!"    `shouldBe` Just ("Hello, World!","")
        runParser (stringP "") "Hello, World!"                 `shouldBe` Just ("","Hello, World!")
        runParser (stringP "World!") "Hello, World!"           `shouldBe` Nothing

      it "multIntsP tests" $ do
        runParser multIntsP "33 * 6"  `shouldBe` Just (198,"")
        runParser multIntsP "33 * 66" `shouldBe` Just (2178,"")
        runParser multIntsP "3 * 6A"  `shouldBe` Just (18,"A")
        runParser multIntsP "3 * 6"   `shouldBe` Just (18,"")
        runParser multIntsP ""        `shouldBe` Nothing

      it "floatP tests" $ do
        runParser floatP "3,14159 some text"  `shouldBe` Just (3.14159," some text")
        runParser floatP "43,14159"           `shouldBe` Just (43.14159,"")
        runParser floatP ""                   `shouldBe` Nothing

      it "multFloatsP tests" $ do
        runParser multFloatsP "0,14159 * 0,122"  `shouldBe` Just (1.727398e-2,"")
        runParser multFloatsP "0,2 * 0,1"        `shouldBe` Just (2.0000001e-2, "") -- какие-то проблемы с округлением
        runParser multFloatsP ""                 `shouldBe` Nothing

      it "simpleExprP tests" $ do
        runParser simpleExprP "3,14 + 2,71"  `shouldBe` Just (SimpleExpr 3.14 '+' 2.71,"")
        runParser simpleExprP "0,14 * 0,1"   `shouldBe` Just (SimpleExpr 0.14 '*' 0.1,"")
        runParser simpleExprP ""             `shouldBe` Nothing

      it "exprP tests" $ do
        runParser exprP "3,14 + 2,71"  `shouldBe` Just (Expr {left = 3.14, op = Sum, right = 2.71},"")
        runParser exprP "0,14 * 0,1"   `shouldBe` Just (Expr {left = 0.14, op = Mult, right = 0.1},"")
        runParser exprP ""             `shouldBe` Nothing

      it "sumMultFloatsP tests" $ do
        runParser sumMultFloatsP "3,14 + 2,71"  `shouldBe` Just (5.8500004,"")
        runParser sumMultFloatsP "0,14 * 0,1"   `shouldBe` Just (1.4e-2,"")
        runParser sumMultFloatsP ""             `shouldBe` Nothing

      it "takeWhileP tests" $ do
        runParser (takeWhileP isDigit) "222jdfj" `shouldBe` Just ("222","jdfj")
        runParser (takeWhileP isDigit) "jdfj222" `shouldBe` Just ("","jdfj222")
        runParser (takeWhileP isDigit) ""        `shouldBe` Just ("","")

      it "takeWhileP tests" $ do
        runParser eofP "222jdfj" `shouldBe` Nothing
        runParser eofP ""        `shouldBe` Just ((),"")

      it "inBetweenP tests" $ do
        runParser (inBetweenP ">" "<" dnaSeqP) ">GATTA<" `shouldBe` Just ([G,A,T,T,A],"")
        runParser (inBetweenP ">" "<" dnaSeqP) "<GATTA>" `shouldBe` Nothing

      it "listP tests" $ do
        runParser (listP symbolsP) "[1, a   , bbb , 23     , 7]"  `shouldBe` Just (["1","a","bbb","23","7"],"")
        runParser (listP symbolsP) "[1, a   , 23     , 7]"        `shouldBe` Just (["1","a","23","7"],"")

      it "valueP tests" $ do
        runParser valueP "342"  `shouldBe` Just (IntValue 342,"")
        runParser valueP "34,2" `shouldBe` Just (FloatValue 34.2,"")
        runParser valueP "AAB"  `shouldBe` Just (StringValue "AAB","")
        runParser valueP ""     `shouldBe` Nothing

      it "abstractRowP tests" $ do
        runParser (abstractRowP ',' intP) "34,55,11,1"     `shouldBe` Just ([34,55,11,1],"")
        runParser (abstractRowP ',' intP) "34,55,11,1,AAA" `shouldBe` Just ([34,55,11,1],",AAA")
        runParser (abstractRowP ',' intP) "AAA"            `shouldBe` Just ([],"AAA")
        runParser (abstractRowP ',' intP) ""               `shouldBe` Just ([],"")

      it "rowP tests" $ do
        runParser (rowP ["Column1", "Column2", "Column3"]) "123, 456, 789" `shouldBe` Just (Row (fromList [("Column1",IntValue 123),("Column2",IntValue 456),("Column3",IntValue 789)]),"")
        runParser (rowP ["Name", "Age", "Score"]) "Bob, 25, 85.5"          `shouldBe` Just (Row (fromList [("Age",IntValue 25),("Name",StringValue "Bob"),("Score",IntValue 85)]),".5")