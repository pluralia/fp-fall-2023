module TestSpec where
import Test.Hspec
import MyLib
import Parser


spec :: Spec
spec = do
    it "Fasta test" $ do
        testParserIO "src/test.fasta" fastaListP `shouldReturn` True

    it "PDB test only_atoms.pdb" $ do
        testParserIO "src/only_atoms.pdb" modelP `shouldReturn` True

    it "PDB test atoms_with_bonds.pdb" $ do
        testParserIO "src/atoms_with_bonds.pdb" modelAndBondP `shouldReturn` True
        
    describe "Parser Monad" $ do
        it "returns Nothing for empty input" $ do
            runParser symbolP "" `shouldBe` Nothing

        it "returns parsed value and remaining string" $ do
            runParser symbolP "abc" `shouldBe` Just ('a', "bc")

        it "binds correctly" $ do
            let parser1 = symbolP
                parser2 = return
            runParser (parser1 >>= parser2) "aa" `shouldBe` Just ('a', "a")

    describe "Maybe' Monad" $ do
        it "returns Nothing' for empty input" $ do
            let m = Nothing' :: Maybe' Int
            m `shouldBe` Nothing'

        it "returns Just' value for non-empty input" $ do
            let m = Just' 5 :: Maybe' Int
            m `shouldBe` Just' 5

        it "binds correctly" $ do
            let m = Just' 5 :: Maybe' Int
                f x = Just' (x * 2)
            (m >>= f) `shouldBe` Just' 10

        it "binds to Nothing' correctly" $ do
            let m = Nothing' :: Maybe' Int
                f x = Just' (x * 2)
            (m >>= f) `shouldBe` Nothing'
            
    describe "List Monad" $ do
            it "returns single element list for pure" $ do
                (getList . pure $ (5 :: Int)) `shouldBe` [5]

            it "applies function correctly" $ do
                let l = List [(2*), (3+)]
                    m = List [1, 2, 3] :: List Int
                getList (l <*> m) `shouldBe` [2, 4, 6, 4, 5, 6]


            it "binds correctly" $ do
                let l = List [1, 2, 3] :: List Int
                    f x = List [x, x * 2]
                getList (l >>= f) `shouldBe` [1, 2, 2, 4, 3, 6]

    describe "Either' Monad" $ do
        it "returns Left' value for Left' input" $ do
            let e = Left' "error" :: Either' String Int
            e `shouldBe` Left' "error"

        it "returns Right' value for Right' input" $ do
            let e = Right' 5 :: Either' String Int
            e `shouldBe` Right' 5

        it "binds correctly for Right' value" $ do
            let e = Right' 5 :: Either' String Int
                f x = Right' (x * 2)
            (e >>= f) `shouldBe` Right' 10

        it "binds correctly for Left' value" $ do
            let e = Left' "error" :: Either' String Int
                f x = Right' (x * 2)
            (e >>= f) `shouldBe` Left' "error"