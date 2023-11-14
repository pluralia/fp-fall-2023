module TestSpec (spec) where

import MyLib
import Parser

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , shouldReturn
  )

spec :: Spec
spec = do
    it "test fastaListP" $ do
        testParserIO "test.fasta" fastaListP `shouldReturn` (True :: Bool)
        testParserIO "commented_test.fasta" fastaListP `shouldReturn` (True :: Bool)
        testParserIO "incorrect_test.fasta" fastaListP `shouldReturn` (False :: Bool)

        runParser fastaListP ">desc1\nSEQ\n;com\n\n\n>desc2\n\n;com2\nLOVE*\nHASKELL" `shouldBe`
            (Just([Fasta "desc1" "SEQ", Fasta "desc2" "LOVE*HASKELL"], "") :: Maybe ([Fasta], String))

    it "test pdbP" $ do
        testParserIO "only_atoms.pdb" pdbP `shouldReturn` (True :: Bool)
        testParserIO "atoms_with_bonds.pdb" pdbP `shouldReturn` (True :: Bool)

        runParser bondP "CONECT  424  255" `shouldBe`
            (Just (PDBBond {
                   bserial1 = 424, bserial2 = 255, bserial3 = Nothing, bserial4 = Nothing, bserial5 = Nothing
                  },"") :: Maybe (PDBBond, String))

        runParser atomP "ATOM     14  H3  ARG I   1       0.279  16.572 -11.188  1.00  0.00           H  " `shouldBe`
            (Just (PDBAtom {
                   aserial = 14,
                   name = " H3 ",
                   altLoc = ' ',
                   resName = "ARG",
                   chainID = 'I',
                   resSeq = 1,
                   iCode = ' ',
                   x = 0.279,
                   y = 16.572,
                   z = -11.188,
                   occupancy = 1.0,
                   tempFactor = 0.0,
                   element = " H",
                   charge = "  "
                  },"") :: Maybe (PDBAtom, String))

        runParser pdbP
            "MODEL        1\n\
            \ATOM      1  N   ARG I   1       1.046  16.244  -0.759  1.00 33.34           N1+\n\
            \CONECT   48  309\n\
            \ENDMDL\n\
            \END" `shouldBe`
            (Just (PDB [PDBModel {
                   mserial = 1,
                   atoms = [PDBAtom {
                      aserial = 1,
                      name = " N  ",
                      altLoc = ' ',
                      resName = "ARG",
                      chainID = 'I',
                      resSeq = 1,
                      iCode = ' ',
                      x = 1.046,
                      y = 16.244,
                      z = -0.759,
                      occupancy = 1.0,
                      tempFactor = 33.34,
                      element = " N",
                      charge = "1+"
                    }],
                   bonds = [PDBBond {
                      bserial1 = 48,
                      bserial2 = 309,
                      bserial3 = Nothing,
                      bserial4 = Nothing,
                      bserial5 = Nothing
                    }]
                  }], "") :: Maybe (PDB, String))
