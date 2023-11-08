module TestSpec (spec) where

import MyLib

import Test.Hspec (
  Spec,
  it,
  describe,
  shouldBe,
  shouldThrow,
  anyErrorCall
 )

import           Control.Exception (evaluate)
import qualified Data.Map.Strict as M

default (Int)

spec :: Spec
spec = do
  describe "Students" $ do
    it "test calculateStudentsLog" $ do
      calculateStudentsLog [Student "a" 1, Student "b" 2, Student "c" 3] `shouldBe` StudentsLog ["c", "b", "a"] (Just 1) (Just 3)
      calculateStudentsLog [Student "a" 3, Student "b" 2] `shouldBe` StudentsLog ["b", "a"] (Just 2) (Just 3)
      calculateStudentsLog [Student "a" 1] `shouldBe` StudentsLog ["a"] (Just 1) (Just 1)
      calculateStudentsLog [] `shouldBe` StudentsLog [] Nothing Nothing

    it "test calculateStudentsLog'" $ do
      calculateStudentsLog' [Student "a" 1, Student "b" 2, Student "c" 3] `shouldBe` StudentsLog ["a", "b", "c"] (Just 1) (Just 3)
      calculateStudentsLog' [Student "a" 3, Student "b" 2] `shouldBe` StudentsLog ["a", "b"] (Just 2) (Just 3)
      calculateStudentsLog' [Student "a" 1] `shouldBe` StudentsLog ["a"] (Just 1) (Just 1)
      calculateStudentsLog' [] `shouldBe` StudentsLog [] Nothing Nothing

  describe "Apples" $ do
    let 
      apTree = Node (Apple "red" 0.3) [Node (Apple "green" 0.2) [Leaf], Node (Apple "red" 0.1) [Leaf]]
      --      ('red', 0.3)
      --     /            \
      -- ('green', 0.2) ('red', 0.1)
      --    |                 |
      --   Leaf              Leaf
      apTree' = Node (Apple "red" 0.3) [Node (Apple "green" 0.2) [Leaf], Node (Apple "red" 0.1) [Leaf, Node (Apple "blue" 0.4) [Leaf]]]
      --       ('red', 0.3)
      --      /            \
      -- ('green', 0.2) ('red', 0.1)
      --    |                |
      --  Leaf           ('blue', 0.4)
      --                     |
      --                   Leaf
      apTree'' = Leaf
      -- Leaf
    it "test applesInRange" $ do
      applesInRange apTree (0.1, 0.3) `shouldBe` True
      applesInRange apTree (0.2, 0.3) `shouldBe` False
      applesInRange apTree' (0.1, 0.4) `shouldBe` True
      applesInRange apTree' (0.2, 0.4) `shouldBe` False
      applesInRange apTree' (0.1, 0.2) `shouldBe` False
      
    it "test heaviestApple" $ do
      heaviestApple apTree `shouldBe` Just (Apple "red" 0.3)
      heaviestApple apTree' `shouldBe` Just (Apple "blue" 0.4)
      heaviestApple apTree'' `shouldBe` Nothing

    it "test thisApple" $ do
      thisApple apTree ["red"] (0.2, 0.3) `shouldBe` Just (Apple "red" 0.3)
      thisApple apTree ["red"] (0.2, 0.2) `shouldBe` Nothing
      thisApple apTree' ["blue"] (0.1, 0.4) `shouldBe` Just (Apple "blue" 0.4)
      thisApple apTree' ["blue"] (0.1, 0.2) `shouldBe` Nothing
      thisApple apTree' ["red", "blue"] (0.2, 0.3) `shouldBe` Just (Apple "red" 0.3)
      thisApple apTree' ["red", "blue"] (0.4, 0.6) `shouldBe` Just (Apple "blue" 0.4)
      thisApple apTree' ["green", "blue"] (0, 0.1) `shouldBe` Nothing
      thisApple apTree'' ["red"] (0.1, 0.3) `shouldBe` Nothing

    it "test sumOfApples" $ do
      sumOfApples apTree `shouldBe` 0.6
      sumOfApples apTree' `shouldBe` 1.0
      sumOfApples apTree'' `shouldBe` 0.0

    it "test collectBasket" $ do
      collectBasket apTree `shouldBe` Basket {apples = M.fromList [("green",[Apple {color = "green", weight = 0.2}]),
                                                                      ("red",[Apple {color = "red", weight = 0.1},Apple {color = "red", weight = 0.3}])]}
      collectBasket apTree' `shouldBe` Basket {apples = M.fromList [("blue",[Apple {color = "blue", weight = 0.4}]),
                                                                      ("green",[Apple {color = "green", weight = 0.2}]),
                                                                      ("red",[Apple {color = "red", weight = 0.1},Apple {color = "red", weight = 0.3}])]}
      collectBasket apTree'' `shouldBe` Basket {apples = M.empty}

  describe "BinHeap" $ do
    let 
      bHeap :: BinaryHeap Int
      bHeap = BinNode 3 (BinNode 2 BinLeaf BinLeaf) (BinNode 4 BinLeaf BinLeaf)
      --          3
      --     /        \
      --     2         4
      --    / \       / \
      -- Leaf Leaf Leaf Leaf
      bHeap' :: BinaryHeap Int
      bHeap' = BinNode 3 (BinNode 2 (BinNode 1 BinLeaf BinLeaf) BinLeaf) (BinNode 4 BinLeaf BinLeaf)
      --          3
      --     /        \
      --     2         4
      --    / \       / \
      --   1  Leaf Leaf Leaf
      --  / \
      -- Leaf Leaf
      bHeap'' :: BinaryHeap Int
      bHeap'' = BinNode 3 (BinNode 2 BinLeaf BinLeaf) BinLeaf
      --      3
      --     / \
      --    2  Leaf
      --   / \
      -- Leaf Leaf
    it "test siftDown" $ do
      siftDown bHeap `shouldBe` BinNode (2 :: Int) (BinNode (3 :: Int) BinLeaf BinLeaf) (BinNode (4 :: Int) BinLeaf BinLeaf)
      siftDown bHeap' `shouldBe` BinNode (2 :: Int) (BinNode (1 :: Int) (BinNode (3 :: Int) BinLeaf BinLeaf) BinLeaf) (BinNode (4 :: Int) BinLeaf BinLeaf)
      siftDown bHeap'' `shouldBe` BinNode (2 :: Int) (BinNode (3 :: Int) BinLeaf BinLeaf) BinLeaf

    it "test buildHeap" $ do
      buildHeap ([3, 2, 4] :: [Int]) `shouldBe` BinNode (2 :: Int) (BinNode (4 :: Int) BinLeaf BinLeaf) (BinNode (3 :: Int) BinLeaf BinLeaf)
      buildHeap ([3, 2, 4, 1] :: [Int]) `shouldBe` BinNode (1 :: Int) (BinNode (2 :: Int) (BinNode (3 :: Int) BinLeaf BinLeaf) BinLeaf) (BinNode (4 :: Int) BinLeaf BinLeaf)
      buildHeap ([3, 2] :: [Int]) `shouldBe` BinNode (2 :: Int) (BinNode (3 :: Int) BinLeaf BinLeaf) BinLeaf
      buildHeap [] `shouldBe` (BinLeaf :: BinaryHeap Int)

  describe "BinTree" $ do
    let
      bTree = branchSize (branchSize (leafSize 'a') (leafSize 'b')) (branchSize (leafSize 'c') (branchSize (leafSize 'd') (leafSize 'e')))
      --      5
      --    /   \
      --   2     3
      --  / \   / \
      -- 1  1  1   2
      -- a  b  c  / \
      --         1   1
      --         d   e
      bTree' = branchPrio (branchPrio (leafPrio 16 'a') (leafPrio 4 'b')) (branchPrio (leafPrio 2 'c') (branchPrio (leafPrio 32 'd') (leafPrio 8 'e')))
      --       2
      --      / \
      --     4   2
      --    / \ / \
      --  16  4 2  8
      --  a   b c  / \
      --          32  8
      --          d   e
      bTree'' = leafSize 'b'
      -- 1
      -- b
    it "test toList" $ do
      toList bTree `shouldBe` "abcde"
      toList bTree'' `shouldBe` "b"

    it "test head'" $ do
      head' bTree `shouldBe` 'a'
      head' bTree'' `shouldBe` 'b'

    it "test getInd" $ do
      getInd bTree 0 `shouldBe` 'a'
      getInd bTree 4 `shouldBe` 'e'
      evaluate (getInd bTree 5) `shouldThrow` anyErrorCall
      getInd bTree'' 0 `shouldBe` 'b'
      evaluate (getInd bTree'' 1) `shouldThrow` anyErrorCall

    it "test getWinner" $ do
      getWinner bTree' `shouldBe` 'c'

    it "test leaf" $ do
      let 
        sizeTree' :: BinaryTree Size' Char
        sizeTree' = leaf 'a'
        -- 1
        -- a

        prioTree' :: BinaryTree Priority' Char
        prioTree' = leaf 'a'
        -- 97
        -- a

      sizeTree' `shouldBe` (BLeaf (Size' 1) 'a' :: BinaryTree Size' Char)
      prioTree' `shouldBe` (BLeaf (Priority' 97) 'a' :: BinaryTree Priority' Char)

    it "test branch" $ do
      let
        sizeTree' :: BinaryTree Size' Char
        sizeTree' = branch (leaf 'a') (leaf 'b')
        --   2
        --  / \
        -- 1   1
        -- a   b

        prioTree' :: BinaryTree Priority' Char
        prioTree' = branch (leaf 'a') (leaf 'b')
        --  97
        -- / \
        -- 97 98  
        -- a  b

      sizeTree' `shouldBe` (BBranch (Size' 2) (BLeaf (Size' 1) 'a') (BLeaf (Size' 1) 'b') :: BinaryTree Size' Char)
      prioTree' `shouldBe` (BBranch (Priority' 97) (BLeaf (Priority' 97) 'a') (BLeaf (Priority' 98) 'b') :: BinaryTree Priority' Char)
