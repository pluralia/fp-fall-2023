module TestSpec (spec) where

import MyLib


import Test.Hspec
  (
    Spec
  , it
  , describe
  , shouldReturn
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "FASTA tests" $ do
        testParserIO "src/test.fasta" fastaListP     `shouldReturn` (True :: Bool)

      it "test pdbP" $ do
        testParserIO "src/only_atoms.pdb"       pdbP `shouldReturn` (True :: Bool)
        testParserIO "src/atoms_with_bonds.pdb" pdbP `shouldReturn` (True :: Bool)

        
        