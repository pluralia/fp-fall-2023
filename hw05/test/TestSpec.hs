module TestSpec where

import MyLib

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )
import qualified Data.Map.Strict as M
import Data.Monoid (Sum(..), Product(..))

spec :: Spec
spec = do
  describe "calculateStudentsLog" $ do
    it "returns empty log for an empty list of students" $ do
      let result = calculateStudentsLog []
      result `shouldBe` StudentsLog { studentNames = [], worstGrade = Nothing, bestGrade = Nothing }

    it "correctly calculates statistics for a list of students" $ do
      let students = [ Student "Alice" 90, Student "Bob" 75, Student "Charlie" 85 ]
      let result = calculateStudentsLog students
      result `shouldBe` StudentsLog { studentNames = ["Alice", "Bob", "Charlie"], worstGrade = Just 75, bestGrade = Just 90 }

  describe "calculateStudentsLog'" $ do
    it "returns empty log for an empty list of students" $ do
      let result = calculateStudentsLog' []
      result `shouldBe` StudentsLog { studentNames = [], worstGrade = Nothing, bestGrade = Nothing }

    it "correctly calculates statistics for a list of students" $ do
      let students = [ Student "Alice" 90, Student "Bob" 75, Student "Charlie" 85 ]
      let result = calculateStudentsLog' students
      result `shouldBe` StudentsLog { studentNames = ["Alice", "Bob", "Charlie"], worstGrade = Just 75, bestGrade = Just 90 }

  describe "Tree Foldable instance" $ do
    it "foldr works correctly for a tree" $ do
      let t = Node 1 [Node 2 [Leaf], Node 3 [Node 4 [Leaf], Leaf]]
      sum t `shouldBe` (10 :: Int)

    it "foldMap works correctly for a tree" $ do
      let t = Node "Hello" [Node " " [Leaf], Node "World" [Node "!" [Leaf], Leaf]]
      concat t `shouldBe` "Hello World!"

    it "toList works correctly for a tree" $ do
      let t = Node 5 [Node 3 [Leaf], Node 7 [Node 6 [Leaf], Leaf]]
      toList' t `shouldBe` [5, 3, 7, 6 :: Int]

    it "returns mempty for Leaf" $
      foldMap Sum Leaf `shouldBe` (Sum 0 :: Sum Int)

    it "sums up all elements in the tree" $
      foldMap Sum testTree `shouldBe` (Sum 15 :: Sum Int)

    it "multiplies all elements in the tree" $
      foldMap Product testTree `shouldBe` (Product 120 :: Product Int)

  describe "applesInRange" $ do
    it "checks if all apples' weights are in the specified range" $ do
      let appleTree = Node (Apple "red" 0.5) [Node (Apple "green" 0.6) [Leaf], Leaf]
      applesInRange appleTree (0.1, 0.7) `shouldBe` True
      applesInRange appleTree (0.6, 0.7) `shouldBe` False

    it "handles an empty tree correctly" $ do
      applesInRange Leaf (0.1, 0.7) `shouldBe` True

  describe "heaviestApple" $ do
    it "finds the heaviest apple in the tree" $ do
      let appleTree = Node (Apple "red" 0.5) [Node (Apple "green" 0.6) [Leaf], Leaf]
      heaviestApple appleTree `shouldBe` Just (Apple "green" 0.6)
      heaviestApple Leaf `shouldBe` Nothing

    it "returns Nothing for an empty tree" $ do
      heaviestApple Leaf `shouldBe` Nothing

  describe "thisApple" $ do
    it "finds an apple with a specified color and weight in the range" $ do
      let appleTree = Node (Apple "red" 0.5) [Node (Apple "green" 0.6) [Leaf], Leaf]
      thisApple appleTree ["green"] (0.6, 0.7) `shouldBe` Just (Apple "green" 0.6)
      thisApple appleTree ["red"] (0.7, 0.8) `shouldBe` Nothing

    it "handles a tree with no matching apples" $ do
      let appleTree = Node (Apple "red" 0.5) [Node (Apple "green" 0.6) [Leaf], Leaf]
      thisApple appleTree ["yellow"] (0.1, 0.9) `shouldBe` Nothing

  describe "sumOfApples" $ do
    it "calculates the sum of weights of all apples in the tree" $ do
      let appleTree = Node (Apple "red" 0.5) [Node (Apple "green" 0.6) [Leaf], Leaf]
      sumOfApples appleTree `shouldBe` 1.1
      sumOfApples Leaf `shouldBe` 0.0

    it "returns 0 for an empty tree" $ do
      sumOfApples Leaf `shouldBe` 0.0

  describe "collectBasket" $ do
    it "collects apples into a basket by color" $ do
      let appleTree = Node (Apple "red" 0.5) [Node (Apple "green" 0.6) [Leaf], Leaf]
          expectedBasket = Basket $ M.fromList [("red", [Apple "red" 0.5]), ("green", [Apple "green" 0.6])]
      collectBasket appleTree `shouldBe` expectedBasket

    it "handles an empty tree correctly" $ do
      collectBasket Leaf `shouldBe` Basket M.empty

    it "collects apples from a tree with single branch" $
      let t = Node (Apple "Red" 5) [Node (Apple "Red" 8) [Node (Apple "Red" 3) [Leaf]]]
          expectedBasket = Basket $ M.singleton "Red" [Apple "Red" 5, Apple "Red" 8, Apple "Red" 3]
      in collectBasket t `shouldBe` expectedBasket

    it "collects apples from a tree with multiple branches and leaves" $
      let t =
            Node (Apple "Red" 10)
              [ Node (Apple "Green" 15) [Leaf]
              , Node (Apple "Red" 20) [Leaf, Leaf]
              , Node (Apple "Yellow" 12) [Node (Apple "Yellow" 8) [Leaf]]
              ]
          expectedBasket = Basket $
            M.fromList
              [ ("Red", [Apple "Red" 10, Apple "Red" 20])
              , ("Green", [Apple "Green" 15])
              , ("Yellow", [Apple "Yellow" 12, Apple "Yellow" 8])
              ]
      in collectBasket t `shouldBe` expectedBasket

  describe "siftDown" $ do
    it "maintains heap property after applying siftDown" $ do
      let heap :: BinaryHeap Int
          heap = BinNode 10 (BinNode 4 BinLeaf BinLeaf) (BinNode 7 BinLeaf BinLeaf)
          expectedHeap :: BinaryHeap Int
          expectedHeap = BinNode 4 (BinNode 10 BinLeaf BinLeaf) (BinNode 7 BinLeaf BinLeaf)
      siftDown heap `shouldBe` expectedHeap

    it "handles heap with only one element correctly" $ do
      let heap :: BinaryHeap Int
          heap = BinNode 5 BinLeaf BinLeaf
      siftDown heap `shouldBe` heap

    it "handles empty list input correctly" $ do
      buildHeap [] `shouldBe` (BinLeaf :: BinaryHeap Int)
    
  describe "toList" $ do
    it "returns elements in order for a simple tree" $ do
      let t :: BinaryTree Int Char
          t = BBranch 1 (BLeaf 2 'A') (BLeaf 3 'B')
      toList t `shouldBe` ['A', 'B']

    it "returns elements in order for a complex tree" $ do
      let t :: BinaryTree Int Char
          t = BBranch 1
                    (BBranch 2 (BLeaf 3 'A') (BBranch 4 (BLeaf 5 'B') (BLeaf 6 'C')))
                    (BLeaf 7 'D')
      toList t `shouldBe` ['A', 'B', 'C', 'D']

  describe "tag" $ do
    it "returns tag value for a leaf node" $ do
      let t :: BinaryTree Int Char
          t = BLeaf 10 'A'
      tag t `shouldBe` 10

    it "returns tag value for a branch node" $ do
      let t :: BinaryTree Int Char 
          t = BBranch 20 (BLeaf 30 'B') (BLeaf 40 'C')
      tag t `shouldBe` 20

  describe "head" $ do
    it "returns the leftmost element for a simple tree" $ do
      let t :: BinaryTree Int Char
          t = BBranch 1 (BLeaf 2 'A') (BLeaf 3 'B')
      head' t `shouldBe` 'A'

    it "returns the leftmost element for a complex tree" $ do
      let t :: BinaryTree Int Char
          t = BBranch 1
                    (BBranch 2 (BLeaf 3 'A') (BBranch 4 (BLeaf 5 'B') (BLeaf 6 'C')))
                    (BLeaf 7 'D')
      head' t `shouldBe` 'A'

  describe "getInd" $ do
    it "returns the value of the first leaf" $ do
      let t = branchSize (branchSize (leafSize 'a') (leafSize 'b')) (branchSize (leafSize 'c') (leafSize 'd'))
      getInd t 1 `shouldBe` 'a'

    it "returns the value of the third leaf in a complex tree" $ do
      let t = branchSize (branchSize (leafSize 'a') (leafSize 'b')) (branchSize (branchSize (leafSize 'c') (leafSize 'd')) (branchSize (leafSize 'e') (leafSize 'f')))
      getInd t 3 `shouldBe` 'c'

  describe "getWinner" $ do
    it "returns the most priority element for a simple tree" $ do
      let simpleTree :: BinaryTree Int Char
          simpleTree = branchPrio (leafPrio 2 'A') (leafPrio 4 'B')
      getWinner simpleTree `shouldBe` 'A'

    it "returns the most priority element for a complex tree" $ do
      let complexTree :: BinaryTree Int Char
          complexTree = branchPrio
                          (branchPrio (leafPrio 16 'A') (leafPrio 4 'B'))
                          (branchPrio (leafPrio 2 'C') (branchPrio (leafPrio 32 'D') (leafPrio 8 'E')))
      getWinner complexTree `shouldBe` 'C'

toList' :: Tree a -> [a]
toList' = foldMap (:[])

testTree :: Tree Int
testTree = Node 1 [ Node 2 [Leaf, Leaf] , Node 3 [Leaf] , Node 4 [Node 5 [Leaf], Leaf]]