module TestSpec (spec) where

import MyLib

import Test.Hspec
  (
    Spec
  , it
  , shouldBe, shouldThrow
  , anyErrorCall, shouldMatchList)

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V
import Control.Exception (evaluate)

spec :: Spec
spec = do
    it "test padZero" $ do
        padZero (T.pack "123456789") 10   `shouldBe` (T.pack "0123456789" :: T.Text)
        padZero (T.pack "hello")     1    `shouldBe` (T.pack "hello"      :: T.Text)
        padZero (T.pack "haskell")   (-1) `shouldBe` (T.pack "haskell"    :: T.Text)
    
    it "test encode" $ do
        encode (T.pack "SECRET") `shouldBe` (T.pack "LTPKTN" :: T.Text)
        encode (T.pack "")       `shouldBe` (T.pack ""       :: T.Text)
        encode (T.pack "ABC")    `shouldBe` (T.pack "XWP"    :: T.Text)
    
    it "test evenodd" $ do
        evenodd "KFTA-M-I-L-Y"                 `shouldBe` (("KT----", "FAMILY")               :: (String, String))
        evenodd "HSE"                          `shouldBe` (("HE", "S")                        :: (String, String))
        evenodd [0, 9, 1, 8, 2, 7, 3, 6, 4, 5] `shouldBe` (([0, 1, 2, 3, 4], [9, 8, 7, 6, 5]) :: ([Int], [Int]))
    
    it "test average" $ do
        average (V.fromList [-3, -2, -1, 0, 1, 2, 3]) - 0.000 < 1e-1 `shouldBe`    (True :: Bool)
        average (V.fromList [1, 1, 2, 3, 5, 8, 13])   - 4.714 < 1e-1 `shouldBe`    (True :: Bool)
        average (V.fromList [1, 2, 4, 8, 16])         - 6.200 < 1e-5 `shouldBe`    (True :: Bool)
        evaluate (average (V.fromList []))                           `shouldThrow` anyErrorCall

    it "test gcContent" $ do
        gcContent (T.pack "ATGCCGAA") - 0.500 < 1e-1 `shouldBe`    (True :: Bool)
        gcContent (T.pack "GCC")      - 1.000 < 1e-1 `shouldBe`    (True :: Bool)
        evaluate (gcContent (T.pack ""))             `shouldThrow` anyErrorCall
    
    it "test isReversePalindrom" $ do
        isReversePalindrom (T.pack "ACT")  `shouldBe` (False :: Bool)
        isReversePalindrom (T.pack "")     `shouldBe` (True  :: Bool)
        isReversePalindrom (T.pack "ACGT") `shouldBe` (True  :: Bool)
        isReversePalindrom (T.pack "TGGC") `shouldBe` (False :: Bool)

    it "test meltingTemp" $ do
        meltingTemp (T.pack "")     `shouldBe` 0
        meltingTemp (T.pack "AC")   `shouldBe` 6
        meltingTemp (T.pack "TGCC") `shouldBe` 14
        meltingTemp (T.pack "AAAA") `shouldBe` 8
    
    it "test identity" $ do
        identity (T.pack "ACG") (T.pack "ACG")   - 1.000 < 1e-1 `shouldBe`    (True :: Bool)
        identity (T.pack "TTGA") (T.pack "TCGA") - 0.750 < 1e-1 `shouldBe`    (True :: Bool)
        evaluate (identity (T.pack "A") (T.pack "CGT"))         `shouldThrow` anyErrorCall
        evaluate (identity (T.pack "") (T.pack ""))             `shouldThrow` anyErrorCall
        
    it "test fromListL" $ do
        fromListL []                      `shouldBe` (M.empty                         :: M.Map Int String)
        fromListL [(1, "A"), (2, "B")]    `shouldBe` (M.fromList [(1, "A"), (2, "B")] :: M.Map Int String)
        fromListL [("Word", 0.1)]         `shouldBe` (M.fromList [("Word", 0.1)]      :: M.Map String Double)
        fromListL [("W", 0.1),("W", 0.2)] `shouldBe` (M.fromList [("W", 0.2)]         :: M.Map String Double)
    
    it "test fromListR" $ do
        fromListR []                      `shouldBe` (M.empty                         :: M.Map Int String)
        fromListR [(1, "A"), (2, "B")]    `shouldBe` (M.fromList [(1, "A"), (2, "B")] :: M.Map Int String)
        fromListR [("Word", 0.1)]         `shouldBe` (M.fromList [("Word", 0.1)]      :: M.Map String Double)
        fromListR [("W", 0.1),("W", 0.2)] `shouldBe` (M.fromList [("W", 0.1)]         :: M.Map String Double)

    it "test nubOrd" $ do
        nubOrd [1, 2, 3, 1]                 `shouldMatchList` ([1, 2, 3]             :: [Int])
        nubOrd []                           `shouldMatchList` ([]                    :: [Int])
        nubOrd ["ACT", "TCA", "TCA", "GCG"] `shouldMatchList` (["ACT", "TCA", "GCG"] :: [String])
    
    it "test buildQuery" $ do
        buildQuery (M.fromList [(T.pack "a", T.pack "1"), (T.pack "b", T.pack "h")]) `shouldBe` (T.pack "b=h&a=1"     :: T.Text)
        buildQuery M.empty                                                           `shouldBe` (T.pack ""            :: T.Text)
        buildQuery (M.fromList [(T.pack "hello", T.pack "world")])                   `shouldBe` (T.pack "hello=world" :: T.Text)
    
    it "test toSymbol" $ do
        toSymbol Glu `shouldBe` ('E' :: Char)
        toSymbol Ter `shouldBe` ('*' :: Char)
        toSymbol Ala `shouldBe` ('A' :: Char)
    
    it "test aminoToStr" $ do
        aminoToStr [Glu, Phe]      `shouldBe` (T.pack "EF"  :: T.Text)
        aminoToStr []              `shouldBe` (T.pack ""    :: T.Text)
        aminoToStr [Arg, Ser, Thr] `shouldBe` (T.pack "RST" :: T.Text)

    it "test translate" $ do
        translate (T.pack "ACTGCATCG")    `shouldBe`    ([Thr, Ala, Ser] :: [AminoAcid])
        translate (T.pack "")             `shouldBe`    ([]              :: [AminoAcid])
        translate (T.pack "TGCACA")       `shouldBe`    ([Cys, Thr]      :: [AminoAcid])
        evaluate (translate (T.pack "T")) `shouldThrow` anyErrorCall
