module TestSpec (spec) where

import MyLib

import Test.Hspec (
  Spec,
  it,
  shouldBe,
  shouldThrow,
  anyErrorCall
 )

import           Control.Exception (evaluate)
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V


spec :: Spec
spec = do
  it "test padZero" $ do
    padZero (T.pack "1") 3  `shouldBe` (T.pack "001" :: T.Text)
    padZero (T.pack "1") 10 `shouldBe` (T.pack "0000000001" :: T.Text)

  it "test encode" $ do 
    encode (T.pack "AAAA") `shouldBe` (T.pack "XXXX" :: T.Text)
    encode (T.pack "ABCD") `shouldBe` (T.pack "XWPC" :: T.Text)

  it "test evenodd" $ do
    evenodd "123456789" `shouldBe` (("13579", "2468") :: ([Char], [Char]))
    evenodd "HDAOTTEA"  `shouldBe` (("HATE", "DOTA") :: ([Char], [Char]))

  it "average" $ do
    average (V.fromList [1..9999]) `shouldBe` (5000 :: Double)
    average (V.fromList [1, 2, 3, 4, 5]) `shouldBe` (3 :: Double)

  it "gcContent" $ do
    gcContent (T.pack "GCTGCAA") `shouldBe` Just (0.5714285714285714 :: Double)
    gcContent (T.pack "TTAAATT") `shouldBe` Just (0.0 :: Double)
    gcContent (T.pack "GCGCGCG") `shouldBe` Just (1.0 :: Double)
    gcContent (T.pack "GCG") `shouldBe` Just (1.0 :: Double)
    gcContent (T.pack "") `shouldBe` Nothing

  it "isReversePalindrom" $ do
    isReversePalindrom (T.pack "GCTGCAA") `shouldBe` False
    isReversePalindrom (T.pack "TATATA") `shouldBe` True
    isReversePalindrom (T.pack "GGGGCCCC") `shouldBe` True
  
  it "meltingTemp" $ do
    meltingTemp (T.pack "GCTGCAA") `shouldBe` (22 :: Int)
    meltingTemp (T.pack "TTAAATT") `shouldBe` (14 :: Int)
    meltingTemp (T.pack "GCGCGCG") `shouldBe` (28 :: Int)
    meltingTemp (T.pack "") `shouldBe` (0 :: Int)

  it "identity" $ do
    identity (T.pack "GCTGC") (T.pack "GCTGC") `shouldBe` (1.0 :: Double)
    identity (T.pack "TTAAATT") (T.pack "GCTGCAA") `shouldBe` (0.0 :: Double)
    identity (T.pack "GCGCGCG") (T.pack "GCTGCAA") `shouldBe` (0.2857142857142857 :: Double)
    evaluate (identity (T.pack "GCG") (T.pack "GCTGCAA")) `shouldThrow` anyErrorCall
    isNaN (identity (T.pack "") (T.pack "")) `shouldBe` True

  it "fromListL" $ do
    fromListL [(1, 2), (3, 4)] `shouldBe` (M.fromList [(1, 2), (3, 4)] :: M.Map Int Int)
    fromListL [('a', 1), ('b', 2), ('c', 3)] `shouldBe` (M.fromList [('a', 1), ('b', 2), ('c', 3)] :: M.Map Char Int)
    fromListL [('a', 1), ('b', 2), ('a', 3)] `shouldBe` (M.fromList [('a', 3), ('b', 2)] :: M.Map Char Int)

  it "fromListR" $ do
    fromListR [(1, 2), (3, 4)] `shouldBe` (M.fromList [(1, 2), (3, 4)] :: M.Map Int Int)
    fromListL [('a', 1), ('b', 2), ('c', 3)] `shouldBe` (M.fromList [('a', 1), ('b', 2), ('c', 3)] :: M.Map Char Int)
    fromListR [('a', 1), ('b', 2), ('a', 3)] `shouldBe` (M.fromList [('a', 1), ('b', 2)] :: M.Map Char Int)

  it "nubOrd" $ do
    nubOrd [1, 2, 3, 4, 5] `shouldBe` ([1, 2, 3, 4, 5] :: [Int])
    nubOrd [1, 2, 3, 4, 5, 1, 2, 3, 4, 5] `shouldBe` ([1, 2, 3, 4, 5] :: [Int])
    nubOrd ['a', 'b', 'c', 'd', 'e'] `shouldBe` ("abcde" :: [Char])
    nubOrd ['a', 'b', 'c', 'd', 'e', 'b', 'c', 'd', 'e'] `shouldBe` ("abcde" :: [Char])

  it "buildQuery" $ do
    buildQuery (M.fromList [(T.pack "a", T.pack "b"), (T.pack "c", T.pack "d")]) `shouldBe` T.pack "c=d&a=b" 
    buildQuery (M.fromList [(T.pack "hello", T.pack "world"), (T.pack "foo", T.pack "bar")]) `shouldBe` T.pack "hello=world&foo=bar"
    buildQuery M.empty `shouldBe` (T.pack "" :: T.Text)

  it "aminoToStr" $ do
    aminoToStr [Ala, Arg, Asn, Asp, Cys, Gln, Glu, Gly, His, Ile, 
                Leu, Lys, Met, Phe, Pro, Pyl, Sec, Ser, Thr, Trp, 
                Tyr, Val, Stp] `shouldBe` (T.pack "ARNDCQEGHILKMFPOUSTWYV*" :: T.Text)
    aminoToStr [] `shouldBe` (T.pack "" :: T.Text)
    
  it "translate" $ do
    translate (T.pack "GCTGCCGCAGCG") `shouldBe` ([Ala, Ala, Ala, Ala] :: [AminoAcid])
    evaluate (translate (T.pack "GCTGCCGCAGCGG")) `shouldThrow` anyErrorCall
    evaluate (translate (T.pack "AUGGCTGCCGCAGCG")) `shouldThrow` anyErrorCall