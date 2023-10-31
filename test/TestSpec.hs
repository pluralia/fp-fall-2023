{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import           Test.Hspec
import           MyLib
import qualified Data.Map        as M
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Data.Set        as S
import Control.Exception (evaluate)
import qualified MyLib as M

spec :: Spec
spec = do

    let seq1 = T.pack "ATGCATGCATGCA"
    let seq2 = T.pack "ATGCTGATCCCATGGGGTACCCATATATA"
    let seq3 = T.pack "AAAATTTT"
    let seq4 = T.pack "ACAATTGT"

    describe "1 - дополнение нулями слева" $ do
        it "padZero" $ do
            padZero seq1 20 `shouldBe` T.pack "0000000ATGCATGCATGCA"
            padZero seq2 20 `shouldBe` seq2
            padZero seq2 32 `shouldBe` T.pack "000ATGCTGATCCCATGGGGTACCCATATATA"

    describe "2 - шифр" $ do
        -- A = X, C = P, G = T, T = L
        it "encode" $ do
            encode seq1 `shouldBe` T.pack "XLTPXLTPXLTPX"
            encode seq2 `shouldBe` T.pack "XLTPLTXLPPPXLTTTTLXPPPXLXLXLX"
            encode seq3 `shouldBe` T.pack "XXXXLLLL"

    describe "3 - четные и нечетные" $ do
        it "evenodd" $ do
            evenodd [1, 2, 3, 4, 5, 6] `shouldBe` (([1, 3, 5], [2, 4, 6]) :: ([Int], [Int]))
            evenodd [1]                `shouldBe` (([1], [])              :: ([Int], [Int]))
            evenodd ["a", "s", "d"]    `shouldBe` ((["a", "d"], ["s"])    :: ([String], [String]))

    describe "4 - cреднее" $ do
        it "average" $ do
            average (V.fromList [1, 2, 3, 4, 5, 6, 7]) `shouldBe`    (4.0  :: Double)
            average (V.fromList [1, 2, 3])             `shouldBe`    (2.0  :: Double)
            average (V.fromList [-1, 1, -2, 2])        `shouldBe`    (0.0  :: Double)
            evaluate (average (V.fromList []))         `shouldThrow` errorCall "Vector is empty!"            

    describe "5 - GC состав" $ do
        it "gcContent" $ do
            gcContent seq1               `shouldBe`    ((6.0 / 13.0)  :: Double)
            gcContent seq2               `shouldBe`    ((13.0 / 29.0) :: Double)
            gcContent seq3               `shouldBe`    (0.0           :: Double)
            evaluate (gcContent T.empty) `shouldThrow` errorCall "Sequence is empty!"

    describe "6 - обратный палиндром" $ do
        it "isRevercePalindrom" $ do
            isReversePalindrom seq1 `shouldBe` False
            isReversePalindrom seq2 `shouldBe` False
            isReversePalindrom seq3 `shouldBe` True

    describe "7 - температура плавления" $ do
        it "meltingTemp" $ do
            meltingTemp seq1 `shouldBe` (38 :: Int)
            meltingTemp seq2 `shouldBe` (84 :: Int)
            meltingTemp seq3 `shouldBe` (16 :: Int)

    describe "8 - мера похожести двух последовательностей" $ do
        it "identity" $ do
            evaluate (identity seq1 seq2)       `shouldThrow` errorCall "Different sequences length!"
            evaluate (identity T.empty T.empty) `shouldThrow` errorCall "Sequences are empty!"
            identity seq1 seq1                  `shouldBe`    (1.0  :: Double)
            identity seq3 seq4                  `shouldBe`    (0.75 :: Double)

    let list1 = [(1, 'A'), (2, 'T'), (3, 'G'), (4, 'C')] :: [(Int, Char)]
    let list2 = [(5, 178)] :: [(Int, Int)]
    let list3 = [] :: [(Int, Int)]

    let map1 = M.fromList list1
    let map2 = M.fromList list2
    let map3 = M.fromList list3

    let list7 = [(1, 'A'), (1, 'B'), (2, 'C'), (2, 'D')] :: [(Int, Char)]
    let map7L = M.fromListL list7
    let map7R = M.fromListR list7

    describe "9 - M.fromList" $ do
        it "fromListL" $ do
            fromListL list1 `shouldBe` map1
            fromListL list2 `shouldBe` map2
            fromListL list3 `shouldBe` map3
            fromListL list7 `shouldBe` map7L
        it "fromListR" $ do
            fromListR list1 `shouldBe` map1
            fromListR list2 `shouldBe` map2
            fromListR list3 `shouldBe` map3
            fromListR list7 `shouldBe` map7R

    let list4 = [1, 2, 3, 3, 3, 4, 5, 5, 6, 7, 7, 7] :: [Int]
    let list5 = [1, 1, 1, 1, 1] :: [Int]
    let list6 = [1, 2, 3] :: [Int]

    let set4 = S.toList (S.fromList list4)
    let set5 = S.toList (S.fromList list5)
    let set6 = S.toList (S.fromList list6)

    describe "10 - уникальные элементы" $ do
        it "nubOrd" $ do
            nubOrd list3 `shouldBe` list3
            nubOrd list4 `shouldBe` set4
            nubOrd list5 `shouldBe` set5
            nubOrd list6 `shouldBe` set6

    let str1 = M.fromList [("a", "1"), ("b", "2"), ("c", "hello")] :: M.Map T.Text T.Text
    let str2 = M.fromList [("d", ":)"), ("e", ":(")]               :: M.Map T.Text T.Text
    let str3 = M.fromList [("holiday ", " happy!")]                :: M.Map T.Text T.Text
    let ans1 = T.pack "a=1&b=2&c=hello"
    let ans2 = T.pack "d=:)&e=:("
    let ans3 = T.pack "holiday = happy!"

    describe "11 - query parameters" $ do
        it "buildQuery" $ do
            buildQuery str1 `shouldBe` ans1
            buildQuery str2 `shouldBe` ans2
            buildQuery str3 `shouldBe` ans3

    let amino1 = [Pyl, Ala, Asp, Ala, Lys, Ala, Arg] :: [AminoAcid]
    let amino2 = [Ala, Asp, Ala, Lys, Ala]           :: [AminoAcid]
    let amino3 = [Trp, Tyr, Stp]                     :: [AminoAcid]
    let code1 = T.pack "OADAKAR"
    let code2 = T.pack "ADAKA"
    let code3 = T.pack "WY*"

    let seq5 = T.pack "AAATTTGGGCCC"
    let seq6 = T.pack "ATC"

    describe "12 - биология..." $ do
        it "список аминокислот в строку из однобуквенного кода: aminoToStr" $ do
            aminoToStr amino1 `shouldBe` code1
            aminoToStr amino2 `shouldBe` code2
            aminoToStr amino3 `shouldBe` code3
        it "translate" $ do
            evaluate (translate seq1) `shouldThrow` errorCall "Uncorrect length sequence!"
            translate seq5 `shouldBe` ([Lys, Phe, Gly, Pro] :: [AminoAcid])
            translate seq6 `shouldBe` ([Ile]                :: [AminoAcid])
