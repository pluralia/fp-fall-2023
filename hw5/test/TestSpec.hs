module TestSpec where

import MyLib
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import Test.Hspec
  ( Spec
  , it
  , shouldBe
  , describe
  )

spec :: Spec
spec = do
    describe "calculateStudentsLog" $ do
        it "worst and best grades for non-empty list of students" $ do
            let students = [ Student "Alice" 80
                            , Student "Bob" 75
                            , Student "Charlie" 100
                            ]
                result = calculateStudentsLog students
                worst = fromJust $ worstGrade result
                best = fromJust $ bestGrade result
            studentNames result `shouldBe` ["Alice", "Bob", "Charlie"]
            worst `shouldBe` 75
            best `shouldBe` 100
    
    describe "calculateStudentsLog'" $ do
        it "worst and best grades for non-empty list of students" $ do
            let students = [ Student "Alice" 80
                            , Student "Bob" 75
                            , Student "Charlie" 100
                            ]
                result = calculateStudentsLog' students
                worst = fromJust $ worstGrade result
                best = fromJust $ bestGrade result
            studentNames result `shouldBe` ["Alice", "Bob", "Charlie"]
            worst `shouldBe` 75
            best `shouldBe` 100

    describe "Tree Foldable instance" $ do
        it "empty tree" $ do
            foldMap (:[]) Leaf `shouldBe` ([] :: [Int])

        it "single node" $ do
            foldMap (:[]) (Node (1 :: Int) []) `shouldBe` [1]

        it "many nodes" $ do
            let tree = Node (10 :: Int) [Node (20 :: Int) [], Node (30 :: Int) [Node (40 :: Int) []]]
            foldMap (:[]) tree `shouldBe` [10,20,30,40]
        
        it "different data types" $ do
            let tree = Node "root" [Node "left" [Node "left.left" []], Node "right" []]
            foldMap (\x -> [x]) tree `shouldBe` ["root", "left", "left.left", "right"]    
    
    describe "applesInRange" $ do
        it "in range" $ do
            let apple1 = Apple "red" 10.0
                apple2 = Apple "green" 2.5
                apple3 = Apple "yellow" 3.0
                tree = Node apple1 [Node apple2 [], Node apple3 []]

            applesInRange tree (2.5, 10.0) `shouldBe` True

        it "out of range" $ do
            let apple1 = Apple "red" 0.0
                apple2 = Apple "green" 2.0
                apple3 = Apple "yellow" 5.0
                tree = Node apple1 [Node apple2 [], Node apple3 []]

            applesInRange tree (0.5, 2.0) `shouldBe` False
        
    
    describe "heaviestApple" $ do
        it "returns the heaviest apple in a tree with multiple apples" $ do
            let apple1 = Apple "red" 1.0
                apple2 = Apple "green" 2.0
                apple3 = Apple "yellow" 1.5
                tree = Node apple1 [Node apple2 [], Node apple3 []]
            heaviestApple tree `shouldBe` Just apple2
    
    describe "maxApple" $ do
        it "returns the other apple when one is Nothing" $ do
            let apple1 = Just (Apple "red" 1.0)
                apple2 = Nothing
            maxApple apple1 apple2 `shouldBe` apple1
        
        it "returns the heaviest apple when both are Just apples" $ do
            let apple1 = Just (Apple "red" 1.5)
                apple2 = Just (Apple "red" 2.5)
            maxApple apple1 apple2 `shouldBe` Just (Apple "red" 2.5)
    
    describe "thisApple" $ do
        let tree = Node (Apple "red" 1.0) [Node (Apple "green" 2.0) [], Node (Apple "yellow" 3.0) []]

        it "returns Just the apple with the specified color and weight range" $ do
            thisApple tree ["green", "yellow"] (1, 3) `shouldBe` Just (Apple "green" 2.0)
        
        it "returns Nothing when no apple matches the specified criteria" $ do
            thisApple tree ["white"] (1, 3) `shouldBe` Nothing

    describe "sumOfApples" $ do
        it "sum of weights for apples in a tree" $ do
            let tree = Node (Apple "red" 1.5) [Node (Apple "green" 2.0) [], Node (Apple "yellow" 3.0) []]
            sumOfApples tree `shouldBe` 6.5

        it "empty tree" $ do
            let emptyTree = Leaf
            sumOfApples emptyTree `shouldBe` 0.0

    describe "collectBasket" $ do
        it "collects apples into the basket by color" $ do
            let tree = Node (Apple "red" 1.0) [Node (Apple "green" 2.0) [], Node (Apple "red" 1.5) []]
                expected = Basket $ M.fromList [("red", [Apple "red" 1.0, Apple "red" 1.5]), ("green", [Apple "green" 2.0])]
            collectBasket tree `shouldBe` expected
    
    describe "siftDown" $ do
        let leaf = BinLeaf :: BinaryHeap Int
            singleNode = BinNode 5 BinLeaf BinLeaf :: BinaryHeap Int
            heap1 = BinNode 3 (BinNode 7 BinLeaf BinLeaf) (BinNode 9 BinLeaf BinLeaf) :: BinaryHeap Int
            heap2 = BinNode 5
                        (BinNode 8 (BinNode 12 BinLeaf BinLeaf) BinLeaf)
                        (BinNode 10 (BinNode 14 BinLeaf BinLeaf) (BinNode 16 BinLeaf BinLeaf)) :: BinaryHeap Int

        it "returns BinLeaf for a leaf node" $ do
            siftDown leaf `shouldBe` BinLeaf

        it "returns the same single node for a single-node heap" $ do
            siftDown singleNode `shouldBe` singleNode

        it "restores the heap property for a small heap" $ do
            let expectedHeap = BinNode 3 (BinNode 7 BinLeaf BinLeaf) (BinNode 9 BinLeaf BinLeaf)
            siftDown heap1 `shouldBe` expectedHeap

        it "restores the heap property for a larger heap with multiple levels" $ do
            let expectedHeap = BinNode 5
                                    (BinNode 8 (BinNode 12 BinLeaf BinLeaf) BinLeaf)
                                    (BinNode 10 (BinNode 14 BinLeaf BinLeaf) (BinNode 16 BinLeaf BinLeaf))
            siftDown heap2 `shouldBe` expectedHeap
    
    describe "toList" $ do
        it "returns a list from a branch" $ do
            toList (BBranch 2 (BLeaf 1 'a') (BLeaf 1 'b')) `shouldBe` ['a', 'b']

        it "returns a list from a branch" $ do
            toList (BBranch 2 (BLeaf 1 'a') (BLeaf 1 'b')) `shouldBe` ['a', 'b']

    describe "tag" $ do
        it "returns tag from a leaf" $ do
            tag (BLeaf 5 'c') `shouldBe` 5

        it "returns tag from a branch" $ do
            tag (BBranch 10 (BLeaf 5 'x') (BLeaf 5 'y')) `shouldBe` 10

    describe "head'" $ do
        it "returns head from a leaf" $ do
            head' (BLeaf 5 'a') `shouldBe` 'a'

    describe "leafPrio" $ do
        it "creates a leaf node with priority and value" $ do
            let leaf = leafPrio 5 'x'
            tag leaf `shouldBe` 5
            getWinner leaf `shouldBe` 'x'

    describe "branchPrio" $ do
        it "creates a branch node with minimum priority" $ do
            let branch = branchPrio (leafPrio 10 'a') (leafPrio 15 'b')
            tag branch `shouldBe` 10
            getWinner branch `shouldBe` 'a'
    
    describe "getInd" $ do
        it "returns the value of the first leaf" $ do
            let t = branchSize
                        (branchSize (leafSize 'a') (leafSize 'b'))
                        (branchSize (leafSize 'c') (leafSize 'd'))
            getInd t 1 `shouldBe` 'a'
        
        it "returns the value of the last leaf" $ do
            let t = branchSize
                        (branchSize (leafSize 'a') (leafSize 'b'))
                        (branchSize (leafSize 'c') (leafSize 'd'))
            getInd t 4 `shouldBe` 'd'

        it "returns the value of the second leaf of the first branch" $ do
            let t = branchSize
                    (branchSize (leafSize 'a') (leafSize 'b'))
                    (branchSize (leafSize 'c') (leafSize 'd'))
            getInd t 2 `shouldBe` 'b'

        it "returns the value of the second leaf of the second branch" $ do
            let t = branchSize
                    (branchSize (leafSize 'a') (leafSize 'b'))
                    (branchSize (leafSize 'c') (leafSize 'd'))
            getInd t 3 `shouldBe` 'c'
    

