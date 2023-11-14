{-# LANGUAGE FlexibleInstances #-}
module TestSpec where

import Test.Hspec
import MyLib
import Parser
import Data.Char       (isUpper)
import Data.Map.Strict (fromList, empty)

spec :: Spec
spec = do
    describe "2. Парсер FASTA" $ do
        it "My test FASTA" $ do
            let testFilePath1 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\test.fasta"
            testParserIO testFilePath1 fastaListP `shouldReturn` True
        
    describe "3. Парсер PDB" $ do -- не советую запускать - уйдет в бесконечный цикл :с
        it "My test PDB" $ do
            let testFilePath2 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\atoms_with_bonds.pdb"
            testParserIO testFilePath2 pdbParser `shouldReturn` True
