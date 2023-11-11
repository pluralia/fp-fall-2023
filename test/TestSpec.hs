module TestSpec where

import Test.Hspec
import Control.Exception (evaluate)
import MyLib
import Parser
import Data.Char       (isUpper)
import Data.Map.Strict (fromList, empty)

spec :: Spec
spec = do
    describe "1. Простые парсеры" $ do
        it "newLineP" $ do
            runParser newLineP "\n"              `shouldBe` Just ('\n', "")
            runParser newLineP "\nqwerty"        `shouldBe` Just ('\n', "qwerty")
            runParser newLineP "hello\nhowudoin" `shouldBe` Nothing
        it "intP" $ do
            runParser intP "12345"   `shouldBe` (Just (12345, "") :: Maybe (Int, String))
            runParser intP "chisl0"  `shouldBe` (Nothing          :: Maybe (Int, String))
            runParser intP "4islo"   `shouldBe` (Just (4, "islo") :: Maybe (Int, String))
            runParser intP "0"       `shouldBe` (Just (0, "")     :: Maybe (Int, String))
        it "dnaSeqP" $ do
            runParser dnaSeqP "ATGC"    `shouldBe` (Just ([A, T, G, C], "") :: Maybe ([DNA], String))
            runParser dnaSeqP "GCSMH"   `shouldBe` (Just ([G, C], "SMH")    :: Maybe ([DNA], String))
            runParser dnaSeqP "nonucls" `shouldBe` (Nothing                 :: Maybe ([DNA], String))
        it "stringP" $ do
            runParser (stringP "line") "line in text"          `shouldBe` (Just ("line", " in text") :: Maybe (String, String))
            runParser (stringP "line") "nothing"               `shouldBe` (Nothing                   :: Maybe (String, String))
            runParser (stringP "") "empty string"              `shouldBe` (Just ("", "empty string") :: Maybe (String, String))


    describe "2. Парсер-комбинаторы и арифметика" $ do
        it "multIntsP" $ do
            runParser multIntsP "55 * 10"      `shouldBe` (Just (550, "")       :: Maybe (Int, String))
            runParser multIntsP "11*11"        `shouldBe` (Just (121, "")       :: Maybe (Int, String))
            runParser multIntsP "0 * 22"       `shouldBe` (Just (0, "")         :: Maybe (Int, String))
            runParser multIntsP "hi! 2*2"      `shouldBe` (Nothing              :: Maybe (Int, String))
            runParser multIntsP "16 * 17 worm" `shouldBe` (Just (272, " worm")  :: Maybe (Int, String))
        it "floatP" $ do
            runParser floatP "12,34"  `shouldBe` (Just (12.34, "") :: Maybe (Float, String))
            runParser floatP "12.34"  `shouldBe` (Nothing          :: Maybe (Float, String))
            runParser floatP "qq12.3" `shouldBe` (Nothing          :: Maybe (Float, String))
            runParser floatP "255"    `shouldBe` (Nothing          :: Maybe (Float, String))
            runParser floatP "qq23,4" `shouldBe` (Nothing          :: Maybe (Float, String))
        it "multFloatsP" $ do
            runParser multFloatsP "12,3 * 45,6"    `shouldBe` (Just (560.88, "")    :: Maybe (Float, String))
            runParser multFloatsP "12.3 * 45,6"    `shouldBe` (Nothing              :: Maybe (Float, String))
            runParser multFloatsP "25,5*15,0 UFO"  `shouldBe` (Just (382.5, " UFO") :: Maybe (Float, String))
            runParser multFloatsP "99 * 88"        `shouldBe` (Nothing              :: Maybe (Float, String))
        it "simpleExprP" $ do
            runParser simpleExprP "12,34 + 56,78" `shouldBe` 
                (Just (SimpleExpr 12.34 '+' 56.78, "") :: Maybe (SimpleExpr, String))
            runParser simpleExprP "12,34 * 56,78" `shouldBe` 
                (Just (SimpleExpr 12.34 '*' 56.78, "") :: Maybe (SimpleExpr, String))
            runParser simpleExprP "12.34 + 56.78" `shouldBe` 
                (Nothing                               :: Maybe (SimpleExpr, String))
        it "sumMultFloatsP" $ do
            runParser sumMultFloatsP "12,299997 + 45,67" `shouldBe` (Just (57.969994, "") :: Maybe (Float, String))
            runParser sumMultFloatsP "12,3 * 45,67"      `shouldBe` (Just (561.74097, "") :: Maybe (Float, String))
            runParser sumMultFloatsP "12.3 * 45.67"      `shouldBe` (Nothing              :: Maybe (Float, String))


    describe "4. Более сложные парсеры" $ do
        it "takeWhileP" $ do
            runParser (takeWhileP isUpper) "ZXCV0ii" `shouldBe` (Just ("ZXCV", "0ii") :: Maybe (String, String))
            runParser (takeWhileP isUpper) "hi! UP"  `shouldBe` (Just ("", "hi! UP")  :: Maybe (String, String))
        it "eofP" $ do
            runParser eofP ""  `shouldBe` (Just ((), "") :: Maybe ((), String))
            evaluate (runParser eofP "some string") `shouldThrow` anyException
        it "inBetweenP" $ do
            runParser (inBetweenP "[" "]" intP) "[999]"     `shouldBe` (Just (999, "") :: Maybe (Int, String))
            runParser (inBetweenP "--" "--" intP) "--666--" `shouldBe` (Just (666, "") :: Maybe (Int, String))
            runParser (inBetweenP "po" "po" intP) "po333po" `shouldBe` (Just (333, "") :: Maybe (Int, String))
        it "listP" $ do 
            runParser (listP symbolsP) "[1, a   , bbb , 23     , 7]" `shouldBe` 
                (Just (["1", "a", "bbb", "23", "7"], "")             :: Maybe ([String], String))
            runParser (listP digitP) "[1, 2, 3, 4, 5]"               `shouldBe` 
                (Just ([1, 2, 3, 4, 5], "")                          :: Maybe ([Int], String))
            runParser (listP multIntsP) "[2*2, 11*11, 1000 * 1]"      `shouldBe` 
                (Just ([4, 121, 1000], "")                           :: Maybe ([Int], String))


    describe "5. Парсим Value" $ do
        it "valueP" $ do
            runParser valueP "1111"     `shouldBe` (Just (IntValue 1111, "")        :: Maybe (Value, String))
            runParser valueP "32,5"     `shouldBe` (Just (FloatValue 32.5, "")      :: Maybe (Value, String))
            runParser valueP "stroka"   `shouldBe` (Just (StringValue "stroka", "") :: Maybe (Value, String))
            runParser valueP ""         `shouldBe` (Nothing                         :: Maybe (Value, String))


    describe "6. Парсим строку CSV формата" $ do
        it "abstractRowP" $ do
            runParser (abstractRowP ';' intP) "1;22;333;4444"               `shouldBe` (Just ([1, 22, 333, 4444], "")                   :: Maybe ([Int], String))
            runParser (abstractRowP '?' symbolsP) "what?why?where?warum^_^" `shouldBe` (Just (["what", "why", "where", "warum"], "^_^") :: Maybe ([String], String))
            runParser (abstractRowP 'w' floatP) ""                          `shouldBe` (Just ([], "")                                   :: Maybe ([Float], String))

        let colName = ["Name", "Age", "Weight", "Base"]
        it "rowP" $ do
            runParser (rowP colName) "Sean, 32, 84, box" `shouldBe`
                (Just (Row (fromList [("Name",   StringValue "Sean"),
                                      ("Age",    IntValue 32),
                                      ("Weight", IntValue 84),
                                      ("Base",   StringValue "box")] ), "")  :: Maybe (Row, String))

            runParser (rowP colName) "Petr,            30, 61       ,       sambo" `shouldBe`
                (Just (Row (fromList [("Name",   StringValue "Petr"),
                                      ("Age",    IntValue 30),
                                      ("Weight", IntValue 61),
                                      ("Base",   StringValue "sambo")] ), "")  :: Maybe (Row, String))

            runParser (rowP colName) "" `shouldBe` (Just (Row empty, "") :: Maybe (Row, String))

