module TestSpec (spec) where

import MyLib

import qualified Data.Map.Strict as M
--import Data.Monoid

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
--  , shouldMatchList
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "StudentsLog tests" $ do
        calculateStudentsLog [] `shouldBe` StudentsLog [] Nothing Nothing
        calculateStudentsLog [Student "Alice" 95, Student "Bob" 78, Student "Charlie" 87] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 78) (Just 95)
        calculateStudentsLog [Student "Alice" 90, Student "Bob" 90, Student "Charlie" 90] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 90) (Just 90)

      it "StudentsLog' tests" $ do
        calculateStudentsLog' [] `shouldBe` StudentsLog [] Nothing Nothing
        calculateStudentsLog' [Student "Alice" 95, Student "Bob" 78, Student "Charlie" 87] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 78) (Just 95)
        calculateStudentsLog' [Student "Alice" 90, Student "Bob" 90, Student "Charlie" 90] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 90) (Just 90)

      it "applesInRange tests" $ do
        applesInRange Leaf (0, 5) `shouldBe` True
        applesInRange (Node (Apple "Red" 3.0) []) (2.0, 4.0) `shouldBe` True
        applesInRange (Node (Apple "Green" 1.5) []) (2.0, 4.0) `shouldBe` False
        applesInRange (Node (Apple "Brown" 2.5) [Node (Apple "Yellow" 2.0) [], Node (Apple "Orange" 3.5) [], Node (Apple "Purple" 4.0) []]) (2.0, 4.0) `shouldBe` True

      it "heaviestApple tests" $ do
        heaviestApple Leaf `shouldBe` Nothing
        heaviestApple (Node (Apple "Red" 3.0) []) `shouldBe` Just (Apple "Red" 3.0)
        heaviestApple (Node (Apple "Red" 0.2) [Node (Apple "Green" 0.15) [], Node (Apple "Yellow" 0.25) []]) `shouldBe` Just (Apple "Yellow" 0.25)

      it "thisApple tests" $ do
        thisApple Leaf ["Red", "Green"] (0.1, 0.3) `shouldBe` Nothing
        thisApple (Node (Apple "Yellow" 0.4) [Node (Apple "Orange" 0.25) [], Node (Apple "Green" 0.15) []]) ["Red", "Blue"] (0.1, 0.3) `shouldBe` Nothing
        thisApple (Node (Apple "Red" 0.2) [Node (Apple "Orange" 0.25) [], Node (Apple "Black" 0.3) []]) ["Red", "Green"] (0.2, 0.3) `shouldBe` Just (Apple "Red" 0.2)

      it "sumOfApples tests" $ do
        sumOfApples Leaf `shouldBe` 0.0
        sumOfApples (Node (Apple "Red" 0.2) []) `shouldBe` 0.2
        sumOfApples (Node (Apple "Red" 0.2) [Node (Apple "Green" 0.3) [], Node (Apple "Yellow" 0.5) []]) `shouldBe` 1.0

      it "collectBasket tests" $ do
        collectBasket Leaf `shouldBe` Basket M.empty
        collectBasket (Node (Apple "Red" 0.2) []) `shouldBe` Basket {apples = M.fromList [("Red",[Apple {color = "Red", weight = 0.2}])]}
        let apple1 = Apple "Red" 0.2
        let apple2 = Apple "Green" 0.3
        let apple3 = Apple "Red" 0.25
        let tree = Node apple1 [Node apple2 [], Node apple3 []]
        collectBasket tree `shouldBe` Basket {apples = M.fromList [("Green",[Apple {color = "Green", weight = 0.3}]),("Red",[Apple {color = "Red", weight = 0.2},Apple {color = "Red", weight = 0.25}])]}
  
      it "siftDown tests" $ do  
        siftDown (BinNode (42 :: Integer) BinLeaf BinLeaf) `shouldBe` BinNode (42 :: Integer) BinLeaf BinLeaf
        siftDown (BinNode (11 :: Integer) (BinNode (10 :: Integer) BinLeaf BinLeaf) (BinNode (8 :: Integer) BinLeaf BinLeaf)) `shouldBe` BinNode (8 :: Integer) (BinNode (10 :: Integer) BinLeaf BinLeaf) (BinNode (11 :: Integer) BinLeaf BinLeaf)

      it "buildHeap tests" $ do  
        buildHeap [3, 1] `shouldBe` BinNode (1 :: Integer) BinLeaf (BinNode (3 :: Integer) BinLeaf BinLeaf)
        buildHeap [3, 1, 5, 2] `shouldBe` BinNode (1 :: Integer) BinLeaf (BinNode (2 :: Integer) BinLeaf (BinNode (3 :: Integer) BinLeaf (BinNode (5 :: Integer) BinLeaf BinLeaf)))

      it "toList tests" $ do  
        toList (BLeaf "tag" "a") `shouldBe` ["a"]
        toList (BBranch "root" (BBranch "left" (BLeaf "l1" "x") (BLeaf "l2" "y")) (BBranch "right" (BLeaf "r1" "z") (BLeaf "r2" "w"))) `shouldBe` ["x", "y", "z", "w"]

      it "tag tests" $ do
        tag (BLeaf "tag" "a") `shouldBe` "tag"
        tag (BBranch "root" (BBranch "left" (BLeaf "l1" "x") (BLeaf "l2" "y")) (BBranch "right" (BLeaf "r1" "z") (BLeaf "r2" "w"))) `shouldBe` "root"

      it "head' tests" $ do
        head' (BLeaf "tag" "a") `shouldBe` "a"
        head' (BBranch "root" (BBranch "left" (BLeaf "l1" "x") (BLeaf "l2" "y")) (BBranch "right" (BLeaf "r1" "z") (BLeaf "r2" "w"))) `shouldBe` "x"
      
      it "head' tests" $ do
        getInd treeWithSizeAnnotations 1 `shouldBe` 'a'
        getInd treeWithSizeAnnotations 2 `shouldBe` 'b'
        getInd treeWithSizeAnnotations 3 `shouldBe` 'c'
        getInd treeWithSizeAnnotations 4 `shouldBe` 'd'
        getInd treeWithSizeAnnotations 5 `shouldBe` 'e'

      it "leafPrio && branchPrio tests" $ do
        leafPrio 2 'a' `shouldBe` BLeaf 2 'a'
        branchPrio (leafPrio 4 'a') (leafPrio 2 'b') `shouldBe` BBranch 2 (leafPrio 4 'a') (leafPrio 2 'b')

      it "returns the winner from a leaf node" $ do
        getWinner (leafPrio 2 'a') `shouldBe` 'a'
        getWinner (branchPrio (leafPrio 2 'a') (leafPrio 4 'b')) `shouldBe` 'a'
        getWinner (branchPrio (leafPrio 4 'b') (leafPrio 2 'a')) `shouldBe` 'a'
        getWinner treeWithPriorityAnnotations `shouldBe` 'c'