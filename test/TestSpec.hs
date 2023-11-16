module TestSpec where

import Test.Hspec
import MyLib
import Parser

isPositive :: Int -> Maybe' Int
isPositive n | n > 0     = Just' n
             | otherwise = Nothing'

generateList :: Int -> List Int
generateList 0 = Null
generateList n = helper 0 n Null
    where
        helper ind n' list | ind == n'  = list
                           | otherwise = helper (ind + 1) n' (Cons ind Null <> list)

f4 :: Int -> Either' a Int
f4 val = Right' (val * 2)

spec :: Spec
spec = do
    describe "1 - Parser scv" $ do

        let col = ["Year", "Make", "Model"]

        it "test.csv" $ do
            testParserIO "test1.csv" (rowP col) `shouldReturn` True
            testParserIO "test2.csv" (rowP col) `shouldReturn` True
            testParserIO "test3.csv" (rowP col) `shouldReturn` True

    describe "2 - Parser Fasta" $ do
        it "test.fasta" $ do
            testParserIO "test.fasta"  fastaListP `shouldReturn` True   
            testParserIO "test2.fasta" fastaListP `shouldReturn` True  
    
    describe "3 - Parser PDB" $ do
        it "only_atoms.pdb" $ do
            testParserIO "only_atoms.pdb" pdbP `shouldReturn` True
        it "atoms_with_bonds.pdb" $ do
            testParserIO "atoms_with_bonds.pdb" pdbP `shouldReturn` True
    
    describe "4 - Monad Parser" $ do
        
        -- let f1 = symbolsP >>= return -- её hlint сказал убрать, что в принципе логично :)
        let f2 = symbolsP >>= stringP
        let f3 s = satisfyP (=='.') *> stringP s

        it "small test for >>=" $ do
            -- runParser f1 "Find.Find" `shouldBe` (Just ("Find",".Find")            :: Maybe (String, String))
            runParser f2 "Find.Find" `shouldBe` (Nothing                          :: Maybe (String, String))
            runParser (symbolsP >>= f3) "Find.Find" `shouldBe` (Just ("Find", "") :: Maybe (String, String))

    describe "5 - Monad for other types" $ do

        it "5a - Maybe'" $ do
            (Just'   50 >>= isPositive) `shouldBe` (Just' 50  :: Maybe' Int)
            (Just'  127 >>= isPositive) `shouldBe` (Just' 127 :: Maybe' Int)
            (Just' (-5) >>= isPositive) `shouldBe` (Nothing'    :: Maybe' Int)
            (Just'    0 >>= isPositive) `shouldBe` (Nothing'    :: Maybe' Int)

        it "5b - List" $ do

            let list1 = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Null))))
            let list2 = Null

            (list1 >>= generateList) `shouldBe` Cons 4 (Cons 3 (Cons 2 (Cons 1 (Cons 0 
                                                     (Cons 3 (Cons 2 (Cons 1 (Cons 0 
                                                             (Cons 2 (Cons 1 (Cons 0 
                                                             (Cons 1 (Cons 0 (Cons 0 Null))
                                                             )))
                                                             ))))
                                                     ))))
                                                    ) 
            (list2 >>= generateList) `shouldBe` Null

        it "5c - Either'" $ do

            (Left'  3 >>= f4) `shouldBe` (Left'  3 :: Either' Int Int)
            (Right' 3 >>= f4) `shouldBe` (Right' 6 :: Either' Int Int)
