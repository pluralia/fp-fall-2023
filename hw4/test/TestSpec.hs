module TestSpec (spec) where

import MyLib

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  , shouldMatchList
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "Returns the original string if its length is greater than or equal to the width and the string with zeros on the left to match the specified width" $ do
        padZero (T.pack "12345") 3 `shouldBe` (T.pack "12345" :: T.Text) 
        padZero (T.pack "42") 5    `shouldBe` (T.pack "00042" :: T.Text)
        
      it "encode tests secret word and empty text" $ do
        encode (T.pack "APPLE") `shouldBe` (T.pack "XBBQT"  :: T.Text)
        encode (T.pack "")      `shouldBe` (T.pack ""       :: T.Text)
        
      it "evenodd tests" $ do
        evenodd "poor-T-IIFK-"  `shouldBe` (("po--IK","orTIF-")          :: (String, String))
        evenodd [0, 1..10]      `shouldBe` (([0,2,4,6,8,10],[1,3,5,7,9]) :: ([Int], [Int]))

      it "average tests" $ do
        average (V.fromList [1, 2, 3, 0, 3, 2, 1])     `shouldBe` (1.7142857142857142 :: Double)
        average (V.fromList [-6, -5, -1, 0, 1, 5, 6])  `shouldBe` (0.0                :: Double)

      it "gcContent tests" $ do
        gcContent (T.pack "AUGGCCCGCAAAAAAAAAAA")      `shouldBe` (0.35 :: Double)
        gcContent (T.pack "GGGCGCGGCGCGCGCCCCCGGGCC")  `shouldBe` (1.0  :: Double)

      it "isReversePalindrom tests" $ do
        isReversePalindrom (T.pack "GCTTTT") `shouldBe` (False :: Bool)
        isReversePalindrom (T.pack "TTTT")   `shouldBe` (False :: Bool)
        isReversePalindrom (T.pack "TTAA")   `shouldBe` (True  :: Bool)
        isReversePalindrom (T.pack "")       `shouldBe` (True  :: Bool)

      it "meltingTemp tests" $ do
        meltingTemp (T.pack "GCCAAA") `shouldBe` (18 :: Int)
        meltingTemp (T.pack "GCC")    `shouldBe` (12 :: Int)
        meltingTemp (T.pack "")       `shouldBe` (0  :: Int)

      it "test identity" $ do
        identity (T.pack "ATTCTTT") (T.pack "ATCGATA") `shouldBe`    (0.42857142857142855 :: Double)
        identity (T.pack "AAAA") (T.pack "AAAA")       `shouldBe`    (1.0 :: Double)
        
      it "fromListL tests" $ do
        fromListL [(4, "H"), (1, "W")] `shouldBe` (M.fromList [(1,"W"),(4,"H")] :: M.Map Int String)
        fromListL [(15, "Apple")]      `shouldBe` (M.fromList [(15,"Apple")]    :: M.Map Int String)
        fromListL []                   `shouldBe` (M.empty                      :: M.Map Int String)

      it "fromListR tests" $ do
        fromListR [(4, "H"), (1, "W")] `shouldBe` (M.fromList [(1,"W"),(4,"H")] :: M.Map Int String)
        fromListR [(15, "Apple")]      `shouldBe` (M.fromList [(15,"Apple")]    :: M.Map Int String)
        fromListR []                   `shouldBe` (M.empty                      :: M.Map Int String)

      it "nubOrd tests" $ do
        nubOrd [1, 2, 3, 1, 3, 2, 1, 4]         `shouldMatchList` ([1, 2, 3, 4]            :: [Int])
        nubOrd ["AATCG", "GCTAT", "GGG", "GGG"] `shouldMatchList` (["AATCG","GCTAT","GGG"] :: [String])
        nubOrd []                               `shouldMatchList` ([]                      :: [Int])

      it "buildQuery tests" $ do
        buildQuery (M.fromList [(T.pack "apple", T.pack "orange")]) `shouldBe` (T.pack "apple=orange" :: T.Text)
        buildQuery M.empty                                          `shouldBe` (T.pack ""             :: T.Text)

      it "buildQuery tests" $ do
        buildQuery (M.fromList [(T.pack "apple", T.pack "orange")]) `shouldBe` (T.pack "apple=orange" :: T.Text)
        buildQuery M.empty                                          `shouldBe` (T.pack ""             :: T.Text)

      it "test toSymbol" $ do
        toSymbol Cys  `shouldBe` ('C' :: Char)
        toSymbol Stop `shouldBe` ('*' :: Char)

      it "test aminoToStr" $ do
        aminoToStr [Ala, Gln, Arg] `shouldBe` (T.pack "AQR" :: T.Text)
        aminoToStr [Cys, Ala]      `shouldBe` (T.pack "CA"  :: T.Text)
        aminoToStr []              `shouldBe` (T.pack ""    :: T.Text)

      it "test translate" $ do
        translate (T.pack "CTACTGTCAGTCATCTAA")  `shouldBe`    ([Leu,Leu,Ser,Val,Ile,Stop] :: [AminoAcid])
        translate (T.pack "CTAACT")              `shouldBe`    ([Leu,Thr]                  :: [AminoAcid])
        translate (T.pack "")                    `shouldBe`    ([]                         :: [AminoAcid])
