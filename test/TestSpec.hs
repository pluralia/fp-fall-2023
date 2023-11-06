module TestSpec where

import Test.Hspec
import MyLib
import Parser
import Data.Char       (isLower)
import Data.Map.Strict (fromList, empty)

spec :: Spec
spec = do
    describe "1. Простые парсеры" $ do
        it "newLineP" $ do
            runParser newLineP "\nHello!" `shouldBe` Just ('\n', "Hello!")
            runParser newLineP "\n53 4 ," `shouldBe` Just ('\n', "53 4 ,")
            runParser newLineP "l\nnkjfv" `shouldBe` Nothing
        it "intP" $ do
            runParser intP "26404" `shouldBe` (Just (26404, "") :: Maybe (Int, String))
            runParser intP "h876 " `shouldBe` (Nothing          :: Maybe (Int, String))
            runParser intP "034."  `shouldBe` (Just (34, ".")   :: Maybe (Int, String))
            runParser intP "0"     `shouldBe` (Just (0, "")     :: Maybe (Int, String))
            runParser intP2 "345h" `shouldBe` (Just (345, "h")  :: Maybe (Int, String))
            runParser intP2 "045"  `shouldBe` (Nothing          :: Maybe (Int, String))
        it "DNA" $ do
            runParser dnaSeqP "ATGCA" `shouldBe` (Just ([A, T, G, C, A], "") :: Maybe ([DNA], String))
            runParser dnaSeqP "CCCHT" `shouldBe` (Just ([C, C, C], "HT")     :: Maybe ([DNA], String))
            runParser dnaSeqP "kjfd6" `shouldBe` (Nothing                    :: Maybe ([DNA], String))
        it "stringP" $ do
            runParser (stringP "find me") "find me again" `shouldBe` (Just ("find me", " again") :: Maybe (String, String))
            runParser (stringP "ghj") "jkl"               `shouldBe` (Nothing                    :: Maybe (String, String))
            runParser (stringP "") "beliberda"            `shouldBe` (Just ("", "beliberda")     :: Maybe (String, String))

    describe "2. Парсер-комбинаторы и арифметика" $ do
        it "multIntsP" $ do
            runParser multIntsP "21 * 10" `shouldBe` (Just (210, "")  :: Maybe (Int, String))
            runParser multIntsP "0*72 lk" `shouldBe` (Just (0, " lk") :: Maybe (Int, String))
            runParser multIntsP "50*70"   `shouldBe` (Just (3500, "") :: Maybe (Int, String))
            runParser multIntsP "b 3 * 7" `shouldBe` (Nothing         :: Maybe (Int, String))
            runParser multIntsP "910 * 1" `shouldBe` (Just (910, "")  :: Maybe (Int, String))
        it "floatP" $ do
            runParser floatP "15,37" `shouldBe` (Just (15.37, "") :: Maybe (Float, String))
            runParser floatP "15.37" `shouldBe` (Nothing          :: Maybe (Float, String))
            runParser floatP "5,37"  `shouldBe` (Nothing          :: Maybe (Float, String))
            runParser floatP "k15.3" `shouldBe` (Nothing          :: Maybe (Float, String))
            runParser floatP "176 l" `shouldBe` (Nothing          :: Maybe (Float, String))
            runParser floatP "017.6" `shouldBe` (Nothing          :: Maybe (Float, String))
        it "multFloatsP" $ do
            runParser multFloatsP "12,1 * 98,76" `shouldBe` (Just (1194.9961, "") :: Maybe (Float, String))
            runParser multFloatsP "34.1*12,4"    `shouldBe` (Nothing              :: Maybe (Float, String))
            runParser multFloatsP "34,1*10,0meh" `shouldBe` (Just (341.0, "meh")  :: Maybe (Float, String))
            runParser multFloatsP "34 * 76"      `shouldBe`  (Nothing             :: Maybe (Float, String))
        it "sumFloatsP" $ do
            runParser sumFloatsP "12,1 + 98,76" `shouldBe` (Just (110.86, "")  :: Maybe (Float, String))
            runParser sumFloatsP "34.1+12,4"    `shouldBe` (Nothing            :: Maybe (Float, String))
            runParser sumFloatsP "34,1+10,0meh" `shouldBe` (Just (44.1, "meh") :: Maybe (Float, String))
            runParser sumFloatsP "34 + 76"      `shouldBe` (Nothing            :: Maybe (Float, String))
        it "simpleExprP" $ do
            runParser simpleExprP "12,1 + 98,76" `shouldBe` 
                (Just (SimpleExpr 12.1 '+' 98.76, "") :: Maybe (SimpleExpr, String))
            runParser simpleExprP "12,1 * 98,76" `shouldBe` 
                (Just (SimpleExpr 12.1 '*' 98.76, "") :: Maybe (SimpleExpr, String))
            runParser simpleExprP "12.1 + 98.76" `shouldBe` 
                (Nothing                              :: Maybe (SimpleExpr, String))
        it "sumMultFloatsP" $ do
            runParser sumMultFloatsP "12,1 + 98,76" `shouldBe` (Just (110.86, "")    :: Maybe (Float, String))
            runParser sumMultFloatsP "12,1 * 98,76" `shouldBe` (Just (1194.9961, "") :: Maybe (Float, String))
            runParser sumMultFloatsP "12.1 * 98,76" `shouldBe` (Nothing              :: Maybe (Float, String))
            runParser sumMultFloatsP "2,7 * 84,6.." `shouldBe` (Nothing              :: Maybe (Float, String))
    
    describe "3. fmap4 и теория" $ do
        let f a b c d = a + b + c + d
        it "fmap4" $ do
            fmap4 f (Just 1) (Just 10) (Just 100) (Just 2000) `shouldBe` (Just 2111 :: Maybe Int)
            fmap4 f (Just 1)  Nothing  (Just 100) (Just 2000) `shouldBe` (Nothing   :: Maybe Int)

    describe "4. Более сложные парсеры" $ do
        it "takeWhileP" $ do
            runParser (takeWhileP isLower) "kfjgO2" `shouldBe` (Just ("kfjg", "O2") :: Maybe (String, String))
            runParser (takeWhileP isLower) "JHDd v" `shouldBe` (Just ("", "JHDd v") :: Maybe (String, String))
        it "eofP" $ do
            runParser eofP ""  `shouldBe` (Just ((), "") :: Maybe ((), String))
            runParser eofP "w" `shouldBe` (Nothing       :: Maybe ((), String))
        it "inBetweenP" $ do
            runParser (inBetweenP "->" "<-" intP) "->707<-" `shouldBe` (Just (707, "") :: Maybe (Int, String))
            runParser (inBetweenP "po" "op" intP) "op707po" `shouldBe` (Nothing        :: Maybe (Int, String))
        it "listP" $ do 
            runParser (listP symbolsP) "[1, a   , bbb , 23     , 7]" `shouldBe` 
                (Just (["1", "a", "bbb", "23", "7"], "")          :: Maybe ([String], String))
            runParser (listP digitP) "[1, 2 , 3 , 5   , 7]" `shouldBe` 
                (Just ([1, 2, 3, 5, 7], "")                       :: Maybe ([Int], String))
            runParser (listP digitsP) "[134,2,37,50,7]" `shouldBe` 
                (Just ([[1, 3, 4], [2], [3, 7], [5, 0], [7]], "") :: Maybe ([[Int]], String))
            runParser (listP multIntsP) "[7*3, 11*1, 100 * 20]" `shouldBe` 
                (Just ([21, 11, 2000], "")                        :: Maybe ([Int], String))
    
    describe "5. Парсим Value" $ do
        it "valueP" $ do
            runParser valueP "5873"           `shouldBe` (Just (IntValue 5873, "")   :: Maybe (Value, String))
            runParser valueP "70,2"           `shouldBe` (Just (FloatValue 70.2, "") :: Maybe (Value, String))
            runParser valueP ""               `shouldBe` (Nothing :: Maybe (Value, String))
            runParser valueP "something else" `shouldBe` 
                (Just (StringValue "something", " else") :: Maybe (Value, String))
    
    describe "6. Парсим строку CSV формата" $ do
        it "abstractRowP" $ do
            runParser (abstractRowP ',' intP) "23, 57, 9, 0all" `shouldBe` 
                (Just ([23, 57, 9, 0], "all")                     :: Maybe ([Int], String))
            runParser (abstractRowP '!' symbolsP) "what!a!beautiful!task-_-" `shouldBe` 
                (Just (["what", "a", "beautiful", "task"], "-_-") :: Maybe ([String], String))
            runParser (abstractRowP '.' floatP) "" `shouldBe` 
                (Just ([], "")                                    :: Maybe ([Float], String))
        let colName = ["Name", "Age", "Sex", "Height"]
        it "rowP" $ do
            runParser (rowP colName) "Mark, 11, Male, 147" `shouldBe`
                (Just (Row (fromList [("Age",    IntValue 11),
                                      ("Height", IntValue 147),
                                      ("Name",   StringValue "Mark"),
                                      ("Sex",    StringValue "Male")] ), "")  :: Maybe (Row, String))
            runParser (rowP colName) "  Ksenya   , 14   , Female  , 162,5    " `shouldBe`
                (Just (Row (fromList [("Age",    IntValue 14),
                                      ("Height", FloatValue 162.5),
                                      ("Name",   StringValue "Ksenya"),
                                      ("Sex",    StringValue "Female")] ), "") :: Maybe (Row, String))
            runParser (rowP colName) "" `shouldBe` (Just (Row empty, "") :: Maybe (Row, String))
            runParser (rowP colName) "Tihon,16,5,Male,170,brother" `shouldBe`
                (Just (Row (fromList [("Age",    FloatValue 16.5),
                                      ("Height", IntValue 170),
                                      ("Name",   StringValue "Tihon"),
                                      ("Sex",    StringValue "Male")] ), "")  :: Maybe (Row, String))
