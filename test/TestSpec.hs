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
  , shouldThrow
  , anyErrorCall
  , shouldMatchList
  )
import Control.Exception (evaluate)

spec :: Spec
spec = do
    it "Task 1. padZero" $ do
        padZero (T.pack "12345") 10 `shouldBe` (T.pack "0000012345" :: T.Text)
        padZero (T.pack "12345") 1  `shouldBe` (T.pack "12345" :: T.Text)
    
    it "Task 2. encode" $ do
        encode (T.pack "ABC") `shouldBe` (T.pack "XWP" :: T.Text)
        encode (T.pack "")    `shouldBe` (T.pack "" :: T.Text)
    
    it "Task 3. evenodd" $ do
        evenodd [1..10]         `shouldBe` (([1,3,5,7,9],[2,4,6,8,10]) :: ([Int], [Int]))
        evenodd "Julia Kostina" `shouldBe` (("JlaKsia","ui otn") :: (String, String))
    
    it "Task 4. average" $ do
        average (V.fromList [-3..3]) `shouldBe` 0.0
        average (V.fromList [1..10]) `shouldBe` 5.5
    
    it "Task 5. gcContent" $ do
        gcContent (T.pack "GATCATGC") `shouldBe` 0.5
        gcContent (T.pack "GGGGCCCC") `shouldBe` 1.0
        gcContent (T.pack "AAATTTA")  `shouldBe` 0.0
    
    it "Task 6. isReversePalindrom" $ do
        isReversePalindrom (T.pack "ACT")  `shouldBe` False
        isReversePalindrom (T.pack "")     `shouldBe` True
        isReversePalindrom (T.pack "ACGT") `shouldBe` True

    it "Task 7. meltingTemp" $ do
        meltingTemp (T.pack "AT")   `shouldBe` 4
        meltingTemp (T.pack "TTTT") `shouldBe` 8
        meltingTemp (T.pack "CGCT") `shouldBe` 14

    it "Task 8. identity" $ do
        identity (T.pack "GATTACA") (T.pack "GATTACA")     `shouldBe` 1.0
        evaluate (identity (T.pack "TTG") (T.pack "TCGA")) `shouldThrow` anyErrorCall

    -- без fromListL
    it "Task 9. fromListR" $ do
        fromListR []                   `shouldBe` (M.empty :: M.Map Int String)
        fromListR [(1, "J"), (2, "K")] `shouldBe` (M.fromList [(1, "J"), (2, "K")] :: M.Map Int String)
        fromListR [("haskell", 10)]    `shouldBe` (M.fromList [("haskell", 10)] :: M.Map String Double)
    
    it "Task 10. nubOrd" $ do
        nubOrd [1, 2, 3, 1] `shouldMatchList` ([1,2,3] :: [Int])
        nubOrd [3, 3, 3, 3, 3, 3, 3] `shouldMatchList` ([3] :: [Int])
        nubOrd [] `shouldMatchList` ([] :: [Int])
    
    -- it "Task 11."

    it "Task 12" $ do
        -- Однобуквенный код
        toSymbol Term `shouldBe` '*'
        toSymbol Ala `shouldBe` 'A'

        -- Строка однобуквенного кода
        aminoToStr [Glu, Term] `shouldBe` (T.pack "E*" :: T.Text)
        aminoToStr [Ala, Asp, Pro, Gln, Term] `shouldBe` (T.pack "ADPQ*" :: T.Text)

        -- Трансляция
        evaluate (translate (T.pack "GATTACA"))   `shouldThrow` anyErrorCall
        translate (T.pack "GGATTACAA") `shouldBe`    ([Gly,Leu,Gln] :: [AminoAcid])
        translate (T.pack "")          `shouldBe`    ([]              :: [AminoAcid])
        translate (T.pack "TGCACATGA") `shouldBe`    ([Cys,Thr,Term]      :: [AminoAcid])
        






