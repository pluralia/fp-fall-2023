module TestSpec (spec) where

import MyLib
import Parser
import Data.Ix


import Test.Hspec
  (
    Spec
  , it
  , shouldReturn
  , shouldBe
  )

spec :: Spec
spec = do
    it "HW3. Task 1. Ix" $ do
        range (Succ Zero, Zero) `shouldBe` []
        range (Succ Zero, Succ Zero) `shouldBe` [Succ Zero]
        range (Zero, Succ (Succ Zero)) `shouldBe` [Zero, Succ Zero, Succ (Succ Zero)]

        index (Zero, Succ Zero) Zero `shouldBe` 0
        index (Zero, Succ Zero) (Succ Zero) `shouldBe` 1
        index (Zero, Succ (Succ Zero)) (Succ (Succ Zero)) `shouldBe` 2

        inRange (Succ Zero, Succ (Succ Zero)) Zero `shouldBe` False
        inRange (Succ Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` True
        inRange (Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` True

    it "HW3. Task 2. InOrder" $ do
        show (In (Node (1 :: Int) [])) `shouldBe` "1"
        show (In (Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [], Node (4 :: Int) []])) `shouldBe` "1234"
        show (In (Node (1 :: Int) [Node (2 :: Int) [Node (4 :: Int) []], Node (3 :: Int) [Node (5 :: Int) [], Node (6 :: Int) []]])) `shouldBe` "124356"

    it "HW6. Task 1. String parser" $ do
        runParser (stringP "HSE") "HSE is the best" `shouldBe` Just ("HSE", " is the best")
        runParser (stringP "HSE") "Is HSE the best?" `shouldBe` Nothing
        runParser (stringP "HSE") "" `shouldBe` Nothing

    it "HW7. Task 2. FASTA" $ do
        testParserIO "src/test.fasta" fastaListP  `shouldReturn` (True :: Bool)