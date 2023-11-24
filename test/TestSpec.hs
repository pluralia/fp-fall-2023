module TestSpec where

import Test.Hspec
import MyLib

spec :: Spec
spec = do
    describe "1. Парсер строки CSV формата with Maybe" $ do
        it "My test CSV" $ do
            let col = ["Year", "Make", "Model"]
            testParserIO "test.csv" (rowP col) `shouldReturn` True

    describe "2. Парсер FASTA" $ do
        it "My test FASTA with comments" $ do
            let testFilePath1 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\fasta_with_comm.fasta"
            testMYParserIO testFilePath1 fastaListP `shouldReturn` True

        it "My test FASTA without comments" $ do
            let testFilePath2 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\fasta_no_comm.fasta"
            testMYParserIO testFilePath2 fastaListP `shouldReturn` True
        
    describe "3. Парсер PDB" $ do 
        it "My test only_atoms" $ do
            let testFilePath3 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\only_atoms.pdb"
            testMYParserIO testFilePath3 pdbParser' `shouldReturn` True

        it "My test atoms_with_bonds" $ do
            let testFilePath4 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\atoms_with_bonds.pdb"
            testMYParserIO testFilePath4 pdbParser `shouldReturn` True
