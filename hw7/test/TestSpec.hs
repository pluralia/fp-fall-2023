module TestSpec (spec) where

import MyLib
import Data.Map.Strict     (fromList)
import Parser

import Test.Hspec
  (
    Spec
  , it
  , describe
  , shouldReturn
  , shouldBe
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "rowP tests" $ do
        runParser (rowP ["Column1", "Column2", "Column3"]) "123, 456, 789" `shouldBe` Just (Row (fromList [("Column1",Just (IntValue 123)),("Column2",Just (IntValue 456)),("Column3",Just (IntValue 789))]),"")
        runParser (rowP ["Name", "Age", "Score"]) "Bob, 25, 85.5"          `shouldBe` Just (Row (fromList [("Age",Just (IntValue 25)),("Name",Just (StringValue "Bob")),("Score",Just (FloatValue 85.5))]),"")

      it "FASTA tests" $ do
        testParserIO "src/test.fasta" fastaListP      `shouldReturn` (True :: Bool)
        testFullyParsedIO "src/test.fasta" fastaListP `shouldReturn` (True :: Bool)
      
      it "test pdbP" $ do
        testParserIO "src/only_atoms.pdb"       pdbP      `shouldReturn` (True :: Bool)
        testParserIO "src/atoms_with_bonds.pdb" pdbP      `shouldReturn` (True :: Bool)
        testFullyParsedIO "src/only_atoms.pdb"       pdbP `shouldReturn` (True :: Bool)
        testFullyParsedIO "src/atoms_with_bonds.pdb" pdbP `shouldReturn` (True :: Bool)

        
        
