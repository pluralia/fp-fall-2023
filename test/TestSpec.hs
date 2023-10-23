module TestSpec (spec) where

import MyLib
import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V


spec :: Spec
spec = do
    it "Lets test padZero" $ do
        padZero (T.pack "12345")           10   `shouldBe` (T.pack "0000012345"        :: T.Text)
        padZero (T.pack "1")               1    `shouldBe` (T.pack "1"                 :: T.Text)
        padZero (T.pack "work_with_text")  1    `shouldBe` (T.pack "work_with_text"    :: T.Text)

    it "Lets test encode" $ do
        encode (T.pack "SLOVO")  `shouldBe` (T.pack "KAFCF"  :: T.Text)
        encode (T.pack "")       `shouldBe` (T.pack ""       :: T.Text)
        encode (T.pack "AAA")    `shouldBe` (T.pack "XXX"    :: T.Text)

    it "Lets test evenodd" $ do
        evenodd [1, 99999, 2, 9999, 3, 999, 4, 99, 5, 9] `shouldBe` (([1, 2, 3, 4, 5], [99999, 9999, 999, 99, 9]) :: ([Int], [Int]))
        evenodd "ittesx t"                               `shouldBe` (("its ", "text")                             :: (String, String))

    it "Lets test average" $ do
        average (V.fromList [-4, -2, -1, 1, 2, 4])    `shouldBe` (0.0 :: Double)
        average (V.fromList [1, 2, 3, 4, 5, 6, 7, 8]) `shouldBe` (4.5 :: Double)

    it "Lets test gcContent" $ do
        gcContent (T.pack "GCCGCGCGCGGCGC") `shouldBe` (1.0 :: Double)
        gcContent (T.pack "ATTATTATATATAA") `shouldBe` (0.0 :: Double)
        gcContent (T.pack "ATGCATGCATGC")   `shouldBe` (0.5 :: Double)

    it "Lets test isReversePalindrom" $ do
        isReversePalindrom (T.pack "")              `shouldBe` (True  :: Bool)
        isReversePalindrom (T.pack "AGCT")          `shouldBe` (True  :: Bool)
        isReversePalindrom (T.pack "TGCGGCTTAAGCT") `shouldBe` (False :: Bool)

    it "Lets test meltingTemp" $ do
        meltingTemp (T.pack "")       `shouldBe` (0 :: Int)
        meltingTemp (T.pack "AATT")   `shouldBe` (8 :: Int)
        meltingTemp (T.pack "GGCC")   `shouldBe` (16 :: Int)
        meltingTemp (T.pack "ATGC")   `shouldBe` (12 :: Int)

    it "Lets test identity" $ do
        identity (T.pack "AGCCAGT") (T.pack "AGTCACC")   `shouldBe` (0.5714285714285714 :: Double)
        identity (T.pack "GGGGG") (T.pack "GGGGG")       `shouldBe` (1.0 :: Double)

    it "Lets test fromListL" $ do
        fromListL []                                  `shouldBe` (M.empty                                        :: M.Map Int String)
        fromListL [(1, "G")]                          `shouldBe` (M.fromList [(1, "G")]                          :: M.Map Int String)
        fromListL [("test", 0.5), ("test", 0.2)]      `shouldBe` (M.fromList [("test", 0.5), ("test", 0.2)]      :: M.Map String Double)

    it "Lets test fromListR" $ do
        fromListR []                                  `shouldBe` (M.empty                                        :: M.Map Int String)
        fromListR [(1, "G")]                          `shouldBe` (M.fromList [(1, "G")]                          :: M.Map Int String)
        fromListR [("test", 0.5), ("test", 0.2)]      `shouldBe` (M.fromList [("test", 0.2), ("test", 0.5)]      :: M.Map String Double)

    it "Lets test nubOrd" $ do
        nubOrd []                                `shouldMatchList` ([]                     :: [Int])
        nubOrd [1, 1, 22, 22, 3]                 `shouldMatchList` ([1, 22, 3]             :: [Int])
        nubOrd ["AA", "AA", "G", "C"]            `shouldMatchList` (["AA", "G", "C"]       :: [String])

    it "Lets test buildQuery" $ do
        buildQuery  M.empty                                                                                         `shouldBe` (T.pack ""                    :: T.Text)
        buildQuery (M.fromList [(T.pack "c", T.pack "hello"), (T.pack "b", T.pack "2"), (T.pack "a", T.pack "1")])  `shouldBe` (T.pack "a=1&b=2&c=hello"     :: T.Text)

    it "Lets test ToSymbol" $ do
        toSymbol Ala  `shouldBe` ('A' :: Char)
        toSymbol Stop `shouldBe` ('*' :: Char)

    it "Lets test aminoToStr" $ do
        aminoToStr [Ala, Arg, Asn] `shouldBe` (T.pack "ARN" :: T.Text)
        aminoToStr []              `shouldBe` (T.pack ""    :: T.Text)

    it "Lets test translate" $ do
        translate (T.pack "TTTTTCTTATTGTCTTCCTCATAG") `shouldBe` ([Phe, Phe, Leu, Leu, Ser, Ser, Ser, Stop] :: [AminoAcid])
        translate (T.pack "")                         `shouldBe` ([]                                        :: [AminoAcid])