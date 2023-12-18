module TestSpec (spec) where

import           Data.Char       (isDigit)
import           Data.Map.Strict (fromList)


import           Test.Hspec      (Spec, describe, it, shouldBe, shouldReturn)

import           MyHW6
import           MyHW7
import           Parser

spec :: Spec
spec = do

-- Task 1
--
  describe "simple parsers from task1" $ do
    it "newLineP" $ do
      runParser newLineP "" `shouldBe` Nothing
      runParser newLineP "abc" `shouldBe` Nothing
      runParser newLineP "\nabc" `shouldBe` Just ('\n', "abc")

    it "intP" $ do
      runParser intP "" `shouldBe` Nothing
      runParser intP "abc" `shouldBe` Nothing
      runParser intP "12abc" `shouldBe` Just (12, "abc")

    it "floatP" $ do
      runParser floatP "" `shouldBe` Nothing
      runParser floatP "abc" `shouldBe` Nothing
      runParser floatP "12abc" `shouldBe` Nothing
      runParser floatP "12.1abc" `shouldBe` Just (12.1 :: Float, "abc")
      runParser floatP "0.abc" `shouldBe` Just (0.0 :: Float, "abc")
      runParser floatP "0.1abc" `shouldBe` Just (0.1 :: Float, "abc")


    it "stringP" $ do
      runParser (stringP "a") "" `shouldBe` Nothing
      runParser (stringP "a") "abc" `shouldBe` Just ("a", "bc")
      runParser (stringP "a") "bce" `shouldBe` Nothing
      runParser (stringP "abc") "abc" `shouldBe` Just ("abc", "")
      runParser (stringP "abc") "abcdef" `shouldBe` Just ("abc", "def")

    it "valueP" $ do
      runParser valueP "" `shouldBe` Nothing
      runParser valueP "123.abc" `shouldBe` Just (FloatValue (123.0 :: Float), "abc")
      runParser valueP "123.123abc" `shouldBe` Just (FloatValue (123.123 :: Float), "abc")
      runParser valueP "123abc" `shouldBe` Just (IntValue (123 :: Int), "abc")
      runParser valueP "abc" `shouldBe` Just (StringValue ("abc" :: String), "")
      runParser valueP "a, bc" `shouldBe` Just (StringValue ("a" :: String), ", bc")

-- Task 2
--
    describe "task 2" $ do
      it "multIntsP" $ do
        runParser multIntsP "1*2"  `shouldBe` Just (2 :: Int, "")
        runParser multIntsP "1*"  `shouldBe` Nothing
        runParser multIntsP "*"  `shouldBe` Nothing
        runParser multIntsP "*1"  `shouldBe` Nothing
        runParser multIntsP "12*2"  `shouldBe` Just (24 :: Int, "")
        runParser multIntsP "12 * 2"  `shouldBe` Just (24 :: Int, "")

      it "multFloatsP" $ do
        runParser multFloatsP "1.0*2.0"  `shouldBe` Just (2.0 :: Float, "")
        runParser multFloatsP "1.11*"  `shouldBe` Nothing
        runParser multFloatsP "*"  `shouldBe` Nothing
        runParser multFloatsP "*1.11"  `shouldBe` Nothing
        runParser multFloatsP "1.5*2. "  `shouldBe` Just (3 :: Float, " ")
        runParser multFloatsP "0.1* 2.abc"  `shouldBe` Just (0.2 :: Float, "abc")

      it "simpleExprP" $ do
        let res1 = SimpleExpr 1.0 '*' 2.0
        let res2 = SimpleExpr 1.0 '+' 2.0
        let res3 = SimpleExpr 1.5 '*' 2.11
        runParser simpleExprP "1.0*2.0 "  `shouldBe` Just (res1, " ")
        runParser simpleExprP "1.0+2.0 "  `shouldBe` Just (res2, " ")
        runParser simpleExprP "1.11*"  `shouldBe` Nothing
        runParser simpleExprP "*"  `shouldBe` Nothing
        runParser simpleExprP "*1.11"  `shouldBe` Nothing
        runParser simpleExprP "1.5*2.11"  `shouldBe` Just (res3, "")

      it "exprP" $ do
        let res1 = Expr 1.0 Mult 2.0
        let res2 = Expr 1.0 Sum 2.0
        let res3 = Expr 1.5 Mult 2.11
        runParser exprP "1.0*2.0 "  `shouldBe` Just (res1, " ")
        runParser exprP "1.0+2.0 "  `shouldBe` Just (res2, " ")
        runParser exprP "1.11*"  `shouldBe` Nothing
        runParser exprP "*"  `shouldBe` Nothing
        runParser exprP "*1.11"  `shouldBe` Nothing
        runParser exprP "1.5*2.11"  `shouldBe` Just (res3, "")

      it "sumMultFloatsP" $ do
        runParser sumMultFloatsP "1.0*2.0 "  `shouldBe` Just (2.0, " ")
        runParser sumMultFloatsP "1.0+2.0 "  `shouldBe` Just (3.0, " ")
        runParser sumMultFloatsP "1.11*"  `shouldBe` Nothing
        runParser sumMultFloatsP "*"  `shouldBe` Nothing
        runParser sumMultFloatsP "*1.11"  `shouldBe` Nothing
        runParser sumMultFloatsP "1.5*2.11"  `shouldBe` Just (3.165, "")

-- Task 4
--
  describe "task 4" $ do
    it "takeWhileP" $ do
      runParser (takeWhileP (== 'a')) "" `shouldBe` Just ("", "")
      runParser (takeWhileP (== 'a')) "bcd" `shouldBe` Just ("", "bcd")
      runParser (takeWhileP (== 'a')) "aaabc" `shouldBe` Just ("aaa", "bc")
      runParser (takeWhileP isDigit) "123ABC" `shouldBe` Just ("123", "ABC")

    it "eofP" $ do
      runParser eofP "" `shouldBe` Just ((), "")
      runParser eofP "abc" `shouldBe` Nothing

    it "inBetweenP" $ do
      runParser (inBetweenP "(" ")" intP) "(123)" `shouldBe` Just (123 :: Int, "")
      runParser (inBetweenP "(" ")" intP) "(abc)" `shouldBe` Nothing
      runParser (inBetweenP "(" ")" intP) "[abc)" `shouldBe` Nothing
      runParser (inBetweenP "(" ")" digitP) "(123)" `shouldBe` Nothing

    it "sepByP" $ do
      let p1 = sepByP intP (satisfyP (== ','))
      let p2 = sepByP floatP (satisfyP (== '|'))
      let p3 = sepByP symbolsP (satisfyP (== ','))
      runParser p1 "1, a   , bbb , 23     , -7" `shouldBe` Nothing
      runParser p1 "1, 22   , 33 , 2" `shouldBe` Just ([1, 22, 33, 2] :: [Int], "")
      runParser p1 "1,22,33,2" `shouldBe` Just ([1, 22, 33, 2] :: [Int], "")
      runParser p2 "1.3 | 0. | 6.12" `shouldBe` Just ([1.3, 0.0, 6.12] :: [Float], "")
      runParser p2 "1.3| 0.| 6.12aba" `shouldBe` Just ([1.3, 0.0, 6.12] :: [Float], "aba")
      runParser p3 "col1,col2,col3\n" `shouldBe` Just (["col1", "col2", "col3"] :: [String], "")

    it "listP" $ do
      let p1 = listP intP
      let p2 = listP symbolsP
      runParser p1 "" `shouldBe` Nothing
      runParser p1 "[]" `shouldBe` Just ([], "")
      runParser p1 "[1,   2,  3,4, 5]" `shouldBe` Just ([1, 2, 3, 4, 5] :: [Int], "")
      runParser p1 "[ 1,   2,  3,4, 5 ]" `shouldBe` Just ([1, 2, 3, 4, 5] :: [Int], "")
      runParser p2 "[ha, ske   , ll]" `shouldBe` Just (["ha", "ske", "ll"] :: [String], "")

    it "rowP" $ do
      let cols1 = ["student", "grade"] :: [String]
      let cols2 = ["Item", "Price", "Count" ]
      let res1 = Row $ fromList [("student", Just $ StringValue "Julia"),
                                   ("grade", Just $ IntValue 5)]
      let res2 = Row $ fromList [("student", Nothing),("grade", Nothing)]
      let res3 = Row $ fromList [("student", Nothing),("grade", Just $ IntValue 5)]
      let res4 = Row $ fromList [("Item", Just $ StringValue "Apple"),
                                 ("Price", Just $ FloatValue 99.99),
                                 ("Count", Just $ IntValue 2)]

      runParser (rowP cols1) "Julia,5" `shouldBe` Just (res1, "")
      runParser (rowP cols1) "," `shouldBe` Just (res2, "")
      runParser (rowP cols1) ",," `shouldBe` Just (res2, "")
      runParser (rowP cols1) ",5" `shouldBe` Just (res3, "")
      runParser (rowP cols1) ",," `shouldBe` Just (res2, "")
      runParser (rowP cols2) "Apple,99.99,2" `shouldBe` Just (res4, "")

-- HW7: task 2

  describe "FASTA" $ do
    it "fullyParsed" $ do
      testFullyParsedIO "files_for_parsing/test.fasta" fastaListP `shouldReturn` True



