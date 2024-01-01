module TestSpec (spec) where

import Data.Map.Strict (fromList)
import HW6
import HW7
import Parser
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = do
  describe "task1" $ do
    it "a" $ do
      runParser newLineP "" `shouldBe` Nothing
      runParser newLineP "abc" `shouldBe` Nothing
      runParser newLineP "\n" `shouldBe` Just ('\n', "")
    it "b" $ do
      runParser intP "" `shouldBe` Nothing
      runParser intP "abc" `shouldBe` Nothing
      runParser intP "123" `shouldBe` Just (123 :: Int, "")
      runParser intP "01230abc" `shouldBe` Just (1230 :: Int, "abc")
      runParser intP "abc1230abc" `shouldBe` Nothing
    it "c" $ do
      runParser floatP "" `shouldBe` Nothing
      runParser floatP "abc" `shouldBe` Nothing
      runParser floatP "0.123" `shouldBe` Just (0.123 :: Float, "")
      runParser floatP "123.123" `shouldBe` Just (123.123 :: Float, "")
      runParser floatP "1230.0abc" `shouldBe` Just (1230.0 :: Float, "abc")
      runParser floatP "abc1230abc" `shouldBe` Nothing
    it "d" $ do
      runParser (stringP "abc") "abc" `shouldBe` Just ("abc", "")
      runParser (stringP "aabc") "abc" `shouldBe` Nothing
      runParser (stringP "abce") "abc" `shouldBe` Nothing
    it "e" $ do
      runParser valueP "0.123abc" `shouldBe` Just (FloatValue (0.123 :: Float), "abc")
      runParser valueP "123abc" `shouldBe` Just (IntValue (123 :: Int), "abc")
      runParser valueP "abc" `shouldBe` Just (StringValue "abc", "")
      runParser valueP "abc123" `shouldBe` Just (StringValue "abc123", "")

  describe "task2" $ do
    it "multDigitsP" $ do
      runParser multDigitsP "4 * 2" `shouldBe` Just (8 :: Int, "")
      runParser multDigitsP "4 * 2a" `shouldBe` Just (8 :: Int, "a")
      runParser multDigitsP "4 *" `shouldBe` Nothing
      runParser multDigitsP "* 2" `shouldBe` Nothing
      runParser multDigitsP " * 2" `shouldBe` Nothing

    it "a" $ do
      runParser multIntsP "11 * 2" `shouldBe` Just (22 :: Int, "")
      runParser multIntsP "1 * 22" `shouldBe` Just (22 :: Int, "")
      runParser multIntsP "11 * 22" `shouldBe` Just (242 :: Int, "")

      runParser multFloatsP "11.0 * 2.0" `shouldBe` Just (22.0 :: Float, "")
      runParser multFloatsP "1.0 * 22.0" `shouldBe` Just (22.0 :: Float, "")
      runParser multFloatsP "11.0 * 22.0" `shouldBe` Just (242.0 :: Float, "")

    -- :p
    it "simpleExprP" $ do
      let res1 = SimpleExpr 1.0 '*' 2.0
      let res2 = SimpleExpr 1.0 '+' 2.0
      let res3 = SimpleExpr 1.5 '*' 2.11
      runParser simpleExprP "1.0*2.0 " `shouldBe` Just (res1, " ")
      runParser simpleExprP "1.0+2.0 " `shouldBe` Just (res2, " ")
      runParser simpleExprP "1.11*" `shouldBe` Nothing
      runParser simpleExprP "*" `shouldBe` Nothing
      runParser simpleExprP "*1.11" `shouldBe` Nothing
      runParser simpleExprP "1.5*2.11" `shouldBe` Just (res3, "")

    it "b" $ do
      runParser sumMultFloatsP "11.0 * 2.0" `shouldBe` Just (22.0 :: Float, "")
      runParser sumMultFloatsP "11.0 + 2.0" `shouldBe` Just (13.0 :: Float, "")
      runParser sumMultFloatsP "11.0 + 2.0a" `shouldBe` Just (13.0 :: Float, "a")

  describe "task4" $ do
    it "a" $ do
      runParser (takeWhileP (== 'a')) "abc" `shouldBe` Just ("a", "bc")
      runParser (takeWhileP (== 'a')) "aaa" `shouldBe` Just ("aaa", "")
      runParser (takeWhileP (== 'b')) "aaa" `shouldBe` Nothing
    it "b" $ do
      runParser eofP "" `shouldBe` Just ((), "")
      runParser eofP "abc" `shouldBe` Nothing
    it "c" $ do
      runParser (inBetweenP "(" ")" intP) "(123)" `shouldBe` Just (123 :: Int, "")
      runParser (inBetweenP "[" ")" intP) "[123)" `shouldBe` Just (123 :: Int, "")
      runParser (inBetweenP "[" ")" intP) "(123)" `shouldBe` Nothing
    it "d" $ do
      let parserSep = satisfyP (== ',')
      let parserMain = intP
      runParser (sepByP' parserMain parserSep) "1, 2, 3" `shouldBe` Just ([1, 2, 3] :: [Int], "")
      runParser (sepByP' parserMain parserSep) "1, 2," `shouldBe` Just ([1, 2] :: [Int], ",")
      runParser (sepByP' parserMain parserSep) "a" `shouldBe` Nothing
      runParser (sepByP' parserMain parserSep) "1, a" `shouldBe` Nothing

    it "listP" $ do
      runParser (listP intP) "[1, 2, 3]" `shouldBe` Just ([1, 2, 3] :: [Int], "")
      runParser (listP intP) "[1, 2, 3)" `shouldBe` Nothing

    -- :p
    it "rowP" $ do
      let cols1 = ["student", "grade"] :: [String]
      let cols2 = ["Item", "Price", "Count"]
      let res1 =
            Row $
              fromList
                [ ("student", Just $ StringValue "Julia"),
                  ("grade", Just $ IntValue 5)
                ]
      let res2 = Row $ fromList [("student", Nothing), ("grade", Nothing)]
      let res3 = Row $ fromList [("student", Nothing), ("grade", Just $ IntValue 5)]
      let res4 =
            Row $
              fromList
                [ ("Item", Just $ StringValue "Apple"),
                  ("Price", Just $ FloatValue 99.99),
                  ("Count", Just $ IntValue 2)
                ]

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