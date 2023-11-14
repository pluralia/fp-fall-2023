module TestSpec (spec) where
    
import Test.Hspec
  (
    Spec
  , it
  , describe
  , shouldReturn
  )

import MyLib

spec :: Spec
spec = do
  describe "simple parsers" $ do
    it "Fasta" $ do
      testParserIO "src/test.fasta" fastaListP `shouldReturn` True

    it "PDB" $ do
      testParserIO "src/only_atoms.pdb" modelP `shouldReturn` True
      testParserIO "src/atoms_with_bonds.pdb" modelP `shouldReturn` True

    
