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
      runParser floatP "6.003" `shouldBe` Just (6.003 :: Float, "")

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

      it "multFloatP" $ do
        runParser multFloatsP "1.0*8.0"  `shouldBe` Just (8.0 :: Float, "")
        runParser multFloatsP "8.0*8.0"  `shouldBe` Just (64.0 :: Float, "")
        runParser multFloatsP "+1"  `shouldBe` Nothing
        runParser multFloatsP "8.0*8.0b"  `shouldBe` Just (64.0 :: Float, "b")
      
      it "simpleExprP" $ do
        let ans = SimpleExpr 10.0 '*' 10.0
        runParser simpleExprP "10.0*10.0 "  `shouldBe` Just (ans, " ")
        runParser simpleExprP "10.11*"  `shouldBe` Nothing
      
      it "exprP" $ do
        let ans = Expr 10.0 Mult 10.0
        runParser exprP "10.0*10.0 "  `shouldBe` Just (ans, " ")
        runParser exprP "10.11*"  `shouldBe` Nothing

      it "sumMultFloatsP" $ do
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

      it "sepByP" $ do
        let s = sepByP symbolsP (satisfyP (== '-'))
        let s1 = sepByP floatP (satisfyP (=='.'))
        runParser s "hello-world-how-are-you" `shouldBe` Just (["hello", "world", "how", "are", "you"], "")
        runParser s1 "3.14.2.7.0.5" `shouldBe` Just ([3.14, 2.7, 0.5] :: [Float], "")
        runParser s "t, r   , b , 8    , , !7" `shouldBe` Nothing
      
      it "listP" $ do
        let s = listP intP
        let s1 = listP symbolsP
        runParser s "" `shouldBe` Nothing
        runParser s1 "[ex,  am, so, oon]" `shouldBe` Just (["ex", "am", "so", "oon"] :: [String], "")
        runParser s "[1, 2, 3,  4, 5]" `shouldBe` Just ([1, 2, 3, 4, 5] :: [Int], "")
-- Task 6
    describe "task6" $ do
      it "rowP" $ do
        let names = ["Name", "Age", "Country" :: String]
        let data1 = "Alice,25,USA"
        let data2 = ",30,Canada"
        let res1 = Row $ fromList [("Name", Just $ StringValue "Alice"), 
                                    ("Age", Just $ IntValue 25), 
                                    ("Country", Just $ StringValue "USA")]
        let res2 = Row $ fromList [("Name", Nothing), 
                                    ("Age", Just $ IntValue 30), 
                                    ("Country", Just $ StringValue "Canada")]
        runParser (rowP names) data1 `shouldBe` Just (res1, "")
        runParser (rowP names) data2 `shouldBe` Just (res2, "")

-- HW 7
  describe "task1" $ do
      it "fastaListP" $ do
        testFullyParsedIO "files_for_parsing/test.fasta" fastaListP `shouldReturn` True


          
