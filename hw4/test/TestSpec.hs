module TestSpec where

import Test.Hspec
import MyLib
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

spec :: Spec
spec = do
-- Task 1
    describe "padZero" $ do
        it "pads the string '123' with zeros on the left to a length of 5" $
            padZero (T.pack "123") 5 `shouldBe` T.pack "00123"

        it "does not change the string '12345' as its length is already 5" $
            padZero (T.pack "12345") 5 `shouldBe` T.pack "12345"
--2 Task
    describe "encode" $ do
        it "encodes the character 'A' as 'Q'" $
            encode (T.pack "A") `shouldBe` T.pack "Q"

        it "returns 'TSTHIQFZ' when input is 'ELEPHANT'" $ do
            encode (T.pack "ELEPHANT") `shouldBe` T.pack "TSTHIQFZ"
--3 Task
    describe "evenodd" $ do
        it "returns empty lists when the input is an empty list" $ do
            evenodd ([] :: [Int]) `shouldBe` ([], [])

        it "returns a list with one element and an empty list when the input is a list with one element" $ do
            evenodd [1 :: Integer] `shouldBe` ([], [1])

        it "returns two lists each with one element when the input is a list with two elements" $ do
            evenodd [1:: Integer, 2:: Integer] `shouldBe` ([2], [1])

        it "returns two lists each with two elements when the input is a list with four elements" $ do
            evenodd [1:: Integer, 2:: Integer, 3:: Integer, 4:: Integer] `shouldBe` ([2, 4], [1, 3])

        it "works with strings" $ do
            evenodd "haskell" `shouldBe` ("akl", "hsel")
--4 Task
    describe "average" $ do
        it "returns the average of a non-empty vector" $ do
            let vec = V.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
            average vec `shouldBe` 3.0

        it "returns 0 for an empty vector" $ do
            let vec = V.empty :: V.Vector Double
            average vec `shouldBe` 0.0
--5 Task
    describe "gcContent" $ do
        it "returns the GC content of a non-empty sequence" $ do
            let seq' = T.pack "GCGCGCAAAA"
            gcContent seq' `shouldBe` 0.6

        it "returns 0 for an empty sequence" $ do
            let seq' = T.empty
            gcContent seq' `shouldBe` 0.0
--6 Task
    describe "isReversePalindrom" $ do
        it "returns True for a reverse palindromic sequence AT" $ do
            let seq1 = T.pack "AT"
            isReversePalindrom seq1 `shouldBe` True

        it "returns False for a non-reverse palindromic sequence" $ do
            let seq' = T.pack "GCTGCAT"
            isReversePalindrom seq' `shouldBe` False
--7 Task
    describe "meltingTemp" $ do
        it "returns 0 for an empty string" $ do
            meltingTemp T.empty `shouldBe` 0

        it "correctly calculates the melting temperature for a non-empty string" $ do
            let dna = T.pack "GCAAT"
            meltingTemp dna `shouldBe` 14
--8 Task
    describe "identity" $ do
        it "returns -1.0 for sequences of different lengths" $ do
            let seq1 = T.pack "AGCCAGT"
            let seq2 = T.pack "AGTCACCA"
            identity seq1 seq2 `shouldBe` -1.0

        it "correctly calculates the identity for sequences of the same length" $ do
            let seq3 = T.pack "AGCCAGT"
            let seq4 = T.pack "AGTCACC"
            identity seq3 seq4 `shouldBe` 0.5714285714285714  -- 4/7
--9 Task
    describe "fromListL" $ do
        it "returns an empty map for an empty list" $ do
            fromListL ([] :: [(String, Int)]) `shouldBe` (M.empty :: M.Map String Int)

        it "correctly creates a map for a non-empty list" $ do
            let lst = [("a", 1), ("b", 2), ("a", 3)]
            fromListL lst `shouldBe` (M.fromList [("a", 3), ("b", 2)] :: M.Map String Int)

    describe "fromListR" $ do
        it "returns an empty map for an empty list" $ do
            fromListR ([] :: [(String, Int)]) `shouldBe` (M.empty :: M.Map String Int)

        it "correctly creates a map for a non-empty list" $ do
            let lst = [("a", 1), ("b", 2), ("a", 3)]
            fromListR lst `shouldBe` (M.fromList [("a", 1), ("b", 2)] :: M.Map String Int)
--10 Task
    describe "nubOrd" $ do
        it "returns an empty list for an empty list" $ do
            nubOrd ([] :: [Int]) `shouldBe` ([] :: [Int])

        it "correctly removes duplicates from a non-empty list" $ do
            let lst = [1, 2, 2, 3, 1, 4]:: [Int]
            nubOrd lst `shouldBe` [1, 2, 3, 4]
--11 Task
    describe "buildQuery" $ do
        it "returns an empty string for an empty map" $ do
            buildQuery (M.empty :: M.Map T.Text T.Text) `shouldBe` T.pack ""

        it "correctly builds a query string from a non-empty map" $ do
            let parameters = M.fromList [(T.pack "a", T.pack "1"), (T.pack "b", T.pack "2"), (T.pack "c", T.pack "hello")]
            buildQuery parameters `shouldBe` T.pack "a=1&b=2&c=hello"
--12 Task b
    describe "toSymbol" $ do
        it "returns the correct symbol for Ala" $ do
            toSymbol Ala `shouldBe` 'A'
        it "returns the correct symbol for Cys" $ do
            toSymbol Cys `shouldBe` 'C'
--12 Task c
    describe "aminoToStr" $ do
        it "returns the correct string for [Ala, Cys]" $ do
            aminoToStr [Ala, Cys] `shouldBe` T.pack "AC"
        it "returns the correct string for [Asp, Glu]" $ do
            aminoToStr [Asp, Glu] `shouldBe` T.pack "DE"
--12 Task d
    describe "translate" $ do
        it "returns the correct amino acids for 'GCTGCG'" $ do
            translate (T.pack "GCTGCG") `shouldBe` Right [Ala, Ala]
        it "returns an error for strings of length not divisible by 3" $ do
            translate (T.pack "GCTGC") `shouldBe` Left "Input is not correct"
        it "returns the correct amino acids for 'GCTTGTTGCGAT'" $ do
            translate (T.pack "GCTTGTTGCGAT") `shouldBe` Right [Ala, Cys, Cys, Asp]