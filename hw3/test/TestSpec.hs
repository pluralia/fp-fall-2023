module TestSpec (spec) where

import MyLib

import Data.Bifunctor (Bifunctor (bimap))
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

default (Int, Double)

spec :: Spec
spec = do
  describe "ChurchNumber" $ do
    it "returns the correct result" $ do
      let x = cn 3
      let y = cn 5
      x == Succ (Succ (Succ Zero)) `shouldBe` True
      y == Succ (Succ (Succ (Succ (Succ Zero)))) `shouldBe` True
      x + y `shouldBe` cn 8
      cn 11513 + cn 8795755 `shouldBe` cn 8807268
      x - y `shouldBe` cn 0
      x * y `shouldBe` cn 15
      cn 65 * cn 43 `shouldBe` cn 2795
      x == y `shouldBe` False
      x < y `shouldBe` True
      x > y `shouldBe` False
      x <= y `shouldBe` True
      x >= y `shouldBe` False
  -- Variable not in scope. Почему? Я не понимаю...
  -- range (cn 1, cn 3) `shouldBe` [cn 1, cn 2, cn 3]
  -- inRange (cn 1, cn 3) (cn 2) `shouldBe` True
  -- inRange (cn 1, cn 3) (cn 4) `shouldBe` False
  -- index (cn 1, cn 3) (cn 2) `shouldBe` 1
  -- index (cn 1, cn 100000) (cn 10000) `shouldBe` 9999
  -- index (cn 1, cn 1000) (cn 40000) `shouldBe` -1

  describe "Tree" $ do
    it "returns the correct result" $ do
      let tree1 = Node "F" [Node "B" [Node "A" [], Node "D" [Node "C" [], Node "E" []]], Node "G" [Node "I" [Node "H" []]]]
      -- something like this
      --      F
      --   /    \
      --  B      G
      -- / \      \
      -- A  D      I
      --   / \      \
      --   C E       H
      let tree11 = Node "F" [Node "B" [Node "A" [], Node "D" [Node "E" [], Node "C" []]], Node "G" [Node "I" [Node "H" []]]]
      -- something like this
      --      F
      --   /    \
      --  B      G
      -- / \      \
      -- A  D      I
      --   / \      \
      --   E C       H
      let tree12 = Node "F" [Node "G" [Node "I" [Node "H" []]], Node "B" [Node "A" [], Node "D" [Node "E" [], Node "C" []]]]
      -- something like this
      --      F
      --   /    \
      --  G      B
      -- /      / \
      -- I     A   D
      --  \       / \
      --   H     E   C
      let tree2 = Node "F" [Node "B" [Node "A" [], Node "D" [Node "C" [], Node "E" []]], Node "G" []]
      -- something like this
      --      F
      --   /    \
      --  B      G
      -- / \
      -- A  D
      --   / \
      --   C E
      let tree3 = Node "B" [Node "A" [], Node "C" []]
      -- something like this
      --  B
      -- / \
      -- A  C
      let tree31 = Node "B" [Node "C" [], Node "A" []]
      -- something like this
      --  B
      -- / \
      -- C  A
      let tree4 = Node "B" []
      -- something like this
      -- B
      show (Pre tree1) `shouldBe` "[F,B,A,D,C,E,G,I,H]"
      show (Pre tree3) `shouldBe` "[B,A,C]"
      show (Pre tree4) `shouldBe` "[B]"
      show (In tree1) `shouldBe` "[A,B,C,D,E,F,H,I,G]"
      show (In tree3) `shouldBe` "[A,B,C]"
      show (In tree4) `shouldBe` "[B]"
      show (Post tree1) `shouldBe` "[A,C,E,D,B,H,I,G,F]"
      show (Post tree3) `shouldBe` "[A,C,B]"
      show (Post tree4) `shouldBe` "[B]"
      tree1 == tree11 `shouldBe` True
      tree1 == tree12 `shouldBe` True
      tree1 == tree2 `shouldBe` False
      tree1 == tree3 `shouldBe` False
      tree1 == tree4 `shouldBe` False
      tree3 == tree31 `shouldBe` True

  describe "CMYK" $ do
    it "returns the correct result" $ do
      -- Есть ли способ не писать :: Int каждый раз?
      toCMYK [0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int] `shouldBe` Just (UnsafeMkCMYK 0 0 0 0)
      toCMYK [100 :: Int, 100 :: Int, 100 :: Int, 100 :: Int] `shouldBe` Just (UnsafeMkCMYK 100 100 100 100)
      toCMYK [0 :: Int, 0 :: Int, 0 :: Int, 101 :: Int] `shouldBe` Nothing
      toCMYK [0 :: Int, 0 :: Int, 0 :: Int] `shouldBe` Nothing
      toCMYK [0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int] `shouldBe` Nothing
      toCMYK [0 :: Int, 0 :: Int, 0 :: Int, -1 :: Int] `shouldBe` Nothing
      (toCMYK . unpack) (mkRGB 0 0 0) `shouldBe` Just (UnsafeMkCMYK 0 0 0 100)
      -- ... but got: Nothing ??????????
      -- ghci> (toCMYK . unpack) (mkRGB 0 0 0) == Just (UnsafeMkCMYK 0 0 0 100)
      -- True
      (toCMYK . unpack) (mkRGB 100 43 123) `shouldBe` Just (UnsafeMkCMYK 19 65 0 52)

  describe "DAY" $ do
    it "returns the correct result" $ do
      nextDay Monday `shouldBe` Tuesday
      nextDay Sunday `shouldBe` Monday
      dayBefore Monday `shouldBe` Sunday
      dayBefore Sunday `shouldBe` Saturday
      daysBeforeWeekend Sunday `shouldBe` 6
      daysBeforeWeekend Saturday `shouldBe` 0
      daysBeforeWeekend Monday `shouldBe` 5

  describe "Functor" $ do
    it "returns the correct result" $ do
      fmap (+ 1) [1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int] `shouldBe` [2, 3, 4, 5]
      fmap (+ 1) (Pair (1 :: Int) (2 :: Int)) `shouldBe` Pair (1 :: Int) (3 :: Int)
      fmap (++ "A") (Node "A" [Node "B" [], Node "C" []]) `shouldBe` Node "AA" [Node "BA" [], Node "CA" []]

  describe "Bifunctor" $ do
    it "returns the correct result" $ do
      bimap (+ 1) (+ 1) (Pair (1 :: Int) (2 :: Int)) `shouldBe` Pair (2 :: Int) (3 :: Int)
      bimap (++ "A") (+ 1) (Pair "A" (2 :: Int)) `shouldBe` Pair "AA" (3 :: Int)
      bimap (+ 1) (++ "A") (Left' (1 :: Int)) `shouldBe` Left' (2 :: Int)
      bimap even (++ "A") (Right' "A") `shouldBe` Right' "AA"