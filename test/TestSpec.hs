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
        it "My test FASTA" $ do
            let testFilePath1 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\test.fasta"
            testMYParserIO testFilePath1 fastaListP `shouldReturn` True
        
    describe "3. Парсер PDB" $ do 
        it "My test only_atoms" $ do
            let testFilePath2 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\only_atoms.pdb"
            testParserIO testFilePath2 pdbParser' `shouldReturn` True

        it "My test atoms_with_bonds" $ do
            let testFilePath3 = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\atoms_with_bonds.pdb"
            testParserIO testFilePath3 pdbParser `shouldReturn` True
