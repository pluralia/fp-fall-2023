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
      let tree = Node 1 [Node 2 [Leaf], Node 3 [Node 4 [Leaf], Leaf]]
      sum tree `shouldBe` (10 :: Int)

    it "foldMap works correctly for a tree" $ do
      let tree = Node "Hello" [Node " " [Leaf], Node "World" [Node "!" [Leaf], Leaf]]
      concat tree `shouldBe` "Hello World!"

    it "toList works correctly for a tree" $ do
      let tree = Node 5 [Node 3 [Leaf], Node 7 [Node 6 [Leaf], Leaf]]
      toList' tree `shouldBe` [5, 3, 7, 6 :: Int]

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

  -- describe "siftDown" $ do
  --   it "maintains heap property after applying siftDown" $ do
  --     let heap = BinNode 10 (BinNode 4 BinLeaf BinLeaf) (BinNode 7 BinLeaf BinLeaf)
  --         expectedHeap = BinNode 4 BinLeaf (BinNode 7 BinLeaf BinLeaf)
  --     siftDown heap `shouldBe` expectedHeap

  --   it "handles heap with only one element correctly" $ do
  --     let heap = BinNode 5 BinLeaf BinLeaf
  --     siftDown heap `shouldBe` heap

  describe "buildHeap" $ do
    it "builds a correct heap from a list of elements" $ do
      let inputList = [9, 4, 7, 5, 2, 1]
          expectedHeap = BinNode 1 (BinNode 4 (BinNode 5 BinLeaf BinLeaf) BinLeaf) (BinNode 7 (BinNode 9 BinLeaf BinLeaf) BinLeaf)
      buildHeap inputList `shouldBe` expectedHeap

    it "handles empty list input correctly" $ do
      buildHeap [] `shouldBe` (BinLeaf :: BinaryHeap Int)


toList' :: Tree a -> [a]
toList' = foldMap (:[])