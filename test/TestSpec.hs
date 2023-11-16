module TestSpec (spec) where

import MyLib
import Parser

import Data.Map.Strict     (fromList)


import Test.Hspec
  (
    Spec
  , it
  , shouldReturn
  , shouldBe
  )

spec :: Spec
spec = do
    it "Task 1. CSV line" $ do
        let colName = ["Year", "Make", "Model"]
        runParser (rowP colName) "1997,Ford,E350" `shouldBe` (Just (Row (fromList [("Make",Just (StringValue "Ford")),
                                                                                   ("Model",Just (StringValue "E350")),
                                                                                   ("Year",Just (IntValue 1997))]),"") :: Maybe (Row, String))

    it "Task 2. FASTA" $ do
        testParserIO "src/test.fasta" fastaListP  `shouldReturn` (True :: Bool)

    it "Task 3. PDB" $ do
        testParserIO "src/only_atoms.pdb"       pdbP `shouldReturn` (True :: Bool)
        testParserIO "src/atoms_with_bonds.pdb" pdbP `shouldReturn` (True :: Bool)