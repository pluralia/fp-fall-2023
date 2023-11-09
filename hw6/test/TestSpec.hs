module TestSpec where

import           Test.Hspec
import           MyLib
import           Parser
import           Data.Maybe          (isNothing, isJust, fromJust)
import           Data.Char
import           Data.Map.Strict     (fromList)

spec :: Spec
spec = do
-- Task1 a
    describe "newLineP" $ do
        it "returns the newline character when the input string starts with a newline" $ do
            runParser newLineP "\nHello" `shouldBe` Just ('\n', "Hello")

        it "returns Nothing when the input string does not start with a newline" $ do
            runParser newLineP "Hello\nWorld" `shouldBe` Nothing

        it "returns Nothing when the input string is empty" $ do
            runParser newLineP "" `shouldBe` Nothing

-- Task1 b
    describe "intP" $ do
        it "returns the parsed integer when the input string starts with an integer" $ do
            runParser intP "123Hello" `shouldBe` Just (123, "Hello")

        it "returns Nothing when the input string does not start with an integer" $ do
            runParser intP "Hello123" `shouldBe` Nothing

        it "returns Nothing when the input string is empty" $ do
            runParser intP "" `shouldBe` Nothing

-- Task1 c
    describe "dnaP" $ do
        it "returns the parsed DNA  when the input string is DNA" $ do
            runParser dnaP "GTCGA" `shouldBe` Just (G, "TCGA")

        it "returns the parsed DNA character when the input string starts with a DNA character" $ do
            runParser dnaP "AHello" `shouldBe` Just (A, "Hello")

        it "returns Nothing when the input string does not start with a DNA character" $ do
            runParser dnaP "HelloA" `shouldBe` Nothing

        it "returns Nothing when the input string is empty" $ do
            runParser dnaP "" `shouldBe` Nothing

-- Task1 d
    describe "stringP" $ do
        it "returns the parsed string when the input string starts with the given string" $ do
            runParser (stringP "Hello") "HelloWorld" `shouldBe` Just ("Hello", "World")

        it "returns Nothing when the input string does not start with the given string" $ do
            runParser (stringP "Hello") "WorldHello" `shouldBe` Nothing

        it "returns Nothing when the input string is empty" $ do
            runParser (stringP "Hello") "" `shouldBe` Nothing

-- Task2 a
    describe "multIntsP" $ do
        it "parses and multiplies two single-digit numbers" $ do
            runParser multIntsP "3 * 4" `shouldBe` Just (12, "")

        it "parses and multiplies two multi-digit numbers" $ do
            runParser multIntsP "33 * 44" `shouldBe` Just (1452, "")

        it "fails to parse when the input is not in the correct format" $ do
            runParser multIntsP "33 44" `shouldBe` Nothing

-- Task2 b
    describe "floatP" $ do
        it "parses float in format `[1..9][0..9]+,[0..9]*`" $ do
            (fst <$> runParser floatP "123,456") `shouldBe` Just 123.456
            (fst <$> runParser floatP "0,789") `shouldBe` Just 0.789
            (fst <$> runParser floatP "1,0") `shouldBe` Just 1.0
            runParser floatP "abc" `shouldSatisfy` isNothing

    describe "multFloatsP" $ do
        it "parses 2 floats and multiplies them" $ do
            (fst <$> runParser multFloatsP "123,456 * 0,789") `shouldBe` Just (123.456 * 0.789)
            (fst <$> runParser multFloatsP "1,0 * 2,0") `shouldBe` Just 2.0
            runParser multFloatsP "abc * def" `shouldSatisfy` isNothing

-- Task2 с
    describe "simpleExprP" $ do
        it "parses addition of two floats" $ do
            runParser simpleExprP  "1,23 + 4,56" `shouldBe` Just (SimpleExpr 1.23 '+' 4.56, "")

        it "parses multiplication of two floats" $ do
            runParser simpleExprP  "7,89 * 0,12" `shouldBe` Just (SimpleExpr 7.89 '*' 0.12, "")

    describe "exprP" $ do
        it "parses addition of two floats" $ do
            runParser exprP "1,23 + 4,56 " `shouldBe` Just (Expr 1.23 Sum 4.56, "")

        it "parses multiplication of two floats" $ do
            runParser exprP "7,89 * 0,12 " `shouldBe` Just (Expr 7.89 Mult 0.12, "")

    describe "sumMultFloatsP" $ do
        it "calculates the sum of two floats" $ do
            runParser sumMultFloatsP "1,23 + 4,56  " `shouldBe` Just (1.23 + 4.56, "")

        it "calculates the product of two floats" $ do
            runParser sumMultFloatsP "7,89 * 0,12 " `shouldBe` Just (7.89 * 0.12, "")

-- Task4 a
    describe "takeWhileP" $ do
        it "returns an empty string when the input is empty" $ do
            runParser (takeWhileP isDigit) "" `shouldBe` Just ("", "")

        it "returns the digits at the beginning of the string" $ do
            runParser (takeWhileP isDigit) "123abc" `shouldBe` Just ("123", "abc")

        it "returns an empty string when the first character does not satisfy the predicate" $ do
            runParser (takeWhileP isDigit) "abc123" `shouldBe` Just ("", "abc123")

-- Task 4 b
    describe "eofP" $ do
        it "returns unit when the input is empty" $ do
            runParser eofP "" `shouldBe` Just ((), "")

-- TAsk 4 c

    describe "inBetweenP" $ do
        it "parses the content between the borders" $ do
            runParser (inBetweenP "[" "]" digitsP) "[123]" `shouldBe` Just ([1,2,3], "")

        it "returns Nothing when the input does not start with the left border" $ do
            runParser (inBetweenP "[" "]" digitsP) "123]" `shouldBe` Nothing

        it "returns Nothing when the input does not end with the right border" $ do
            runParser (inBetweenP "[" "]" digitsP) "[123" `shouldBe` Nothing

-- Task4 d
    describe "listP" $ do
        it "parses an empty list" $ do
            runParser (listP floatP) "[]" `shouldBe` Just ([], "")

        it "parses a list of floating point numbers" $ do
            runParser (listP floatP) "[1,0, 2,0, 3,0]" `shouldBe` Just ([1.0, 2.0, 3.0], "")

        it "returns Nothing when the input is not a properly formatted list" $ do
            runParser (listP floatP) "[1,0, 2,0, 3,0" `shouldBe` Nothing
    
-- Task 5
    describe "valueP" $ do
        it "parses an integer value" $ do
            let result = runParser valueP "123"
            result `shouldSatisfy` isJust
            fst (fromJust result) `shouldBe` IntValue 123

        it "parses a float value" $ do
            let result = runParser valueP "123,45"
            result `shouldSatisfy` isJust
            fst (fromJust result) `shouldBe` FloatValue 123.45

        it "parses a string value" $ do
            let result = runParser valueP "hello"
            result `shouldSatisfy` isJust
            fst (fromJust result) `shouldBe` StringValue "hello"

-- Task 6 a
    describe "abstractRowP" $ do
        it "parses a row of integers" $ do
            let result = runParser (abstractRowP ',' intP) "1,2,3"
            result `shouldSatisfy` isJust
            fst (fromJust result) `shouldBe` [1,2,3]

        it "parses a row of strings" $ do
            let result = runParser (abstractRowP ',' symbolsP) "apple,banana,carrot"
            result `shouldSatisfy` isJust
            fst (fromJust result) `shouldBe` ["apple","banana","carrot"]

-- Task 6 b
    describe "rowP" $ do
        it "parses a row into a Map" $ do
            let result = runParser (rowP ["name", "age"]) "John,30"
            result `shouldSatisfy` isJust
            fst (fromJust result) `shouldBe` Row (fromList [("name", StringValue "John"), ("age", IntValue 30)])

        it "parses a row into a Map" $ do
            let result = runParser (rowP ["apple", "colour"]) "number1,yellow"
            result `shouldSatisfy` isJust
            fst (fromJust result) `shouldBe` Row (fromList [("apple", StringValue "number1"), ("colour", StringValue "yellow")])


