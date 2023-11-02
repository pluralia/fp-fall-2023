module TestSpec where

import Test.Hspec
import MyLib
import Data.Monoid ()
import qualified Data.Map.Strict as M
import Data.Array ()

spec :: Spec
spec = do
    -- Task2
    describe "calculateStudentsLog" $ do
        it "calculates the students log correctly" $ do
            let students = [Student "Alice" 85, Student "Bob" 90, Student "Charlie" 80]
            calculateStudentsLog students `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 80) (Just 90)

    describe "calculateStudentsLog'" $ do
        it "calculates the students log correctly" $ do
            let students = [Student "Alice" 85, Student "Bob" 90, Student "Charlie" 80]
            calculateStudentsLog' students `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 80) (Just 90)

-- Task3
    describe "Tree Foldable instance" $ do
        it "folds an empty tree" $ do
            foldMap (:[]) Leaf `shouldBe` ([] :: [Int])

        it "folds a tree with one node" $ do
            foldMap (:[]) (Node (1 :: Int) []) `shouldBe` [1]

        it "folds a tree with multiple nodes" $ do
            let tree = Node (1 :: Int) [Node (2 :: Int) [], Node (3 :: Int) [Node (4 :: Int) []]]
            foldMap (:[]) tree `shouldBe` [1,2,3,4]

-- Task4 a
    describe "applesInRange" $ do
        it "returns True when all apples are in range" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "yellow" 3.0
            let tree = Node apple1 [Node apple2 [], Node apple3 []]

            applesInRange tree (1.0, 3.0) `shouldBe` True

        it "returns False when not all apples are in range" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "yellow" 3.0
            let tree = Node apple1 [Node apple2 [], Node apple3 []]

            applesInRange tree (1.5, 2.5) `shouldBe` False
-- Task4 b
    describe "heaviestApple" $ do
        it "returns the heaviest apple in the tree" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "yellow" 3.0
            let tree = Node apple1 [Node apple2 [], Node apple3 []]

            heaviestApple tree `shouldBe` Just apple3

        it "returns Nothing for an empty tree" $ do
            heaviestApple Leaf `shouldBe` Nothing

-- Task4 c
    describe "thisApple" $ do
        it "returns an apple that matches the color and weight criteria" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "yellow" 3.0
            let tree = Node apple1 [Node apple2 [], Node apple3 []]

            thisApple tree ["red", "green"] (1.0, 2.0) `shouldBe` Just apple2

        it "returns Nothing when no apples match the criteria" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "yellow" 3.0
            let tree = Node apple1 [Node apple2 [], Node apple3 []]
            
            thisApple tree ["blue"] (4.0, 5.0) `shouldBe` Nothing
-- Task4 d
    describe "sumOfApples" $ do
        it "returns the sum of weights of all apples in the tree" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "yellow" 3.0
            let tree = Node apple1 [Node apple2 [], Node apple3 []]

            sumOfApples tree `shouldBe` 6.0

        it "returns 0 for an empty tree" $ do
        
            sumOfApples Leaf `shouldBe` 0.0

-- Task5
    describe "collectBasket" $ do
        it "collects apples into a basket grouped by color and sorted by weight with apples with same color" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "red" 3.0

            let tree = Node apple1 [Node apple2 [], Node apple3 []]
            let expectedBasket = Basket $ M.fromList [("red", [apple1, apple3]), ("green", [apple2])]
            collectBasket tree `shouldBe` expectedBasket

        it "collects apples into a basket grouped by color and sorted by weight with apples with different color" $ do
            let apple1 = Apple "red" 1.0
            let apple2 = Apple "green" 2.0
            let apple3 = Apple "yellow" 3.0

            let tree = Node apple1 [Node apple2 [], Node apple3 []]
            let expectedBasket = Basket $ M.fromList [("red", [apple1]), ("green", [apple2]), ("yellow", [apple3])]
            collectBasket tree `shouldBe` expectedBasket

        it "returns an empty basket for an empty tree" $ do
            collectBasket Leaf `shouldBe` Basket M.empty
    
-- Task6 a
    describe "siftDown" $ do
        it "handles an empty heap" $ do
            siftDown (BinLeaf :: BinaryHeap Integer) `shouldBe` (BinLeaf :: BinaryHeap Integer)

        it "handles a heap with one element" $ do
            let heap = BinNode 1 BinLeaf BinLeaf :: BinaryHeap Integer
            siftDown heap `shouldBe` heap

        it "handles a heap where the root is larger than one child" $ do
            let heap = BinNode 3 (BinNode 1 BinLeaf BinLeaf) (BinNode 2 BinLeaf BinLeaf) :: BinaryHeap Integer
            let expectedHeap = BinNode 1 (BinNode 3 BinLeaf BinLeaf) (BinNode 2 BinLeaf BinLeaf) :: BinaryHeap Integer
            siftDown heap `shouldBe` expectedHeap

-- Task6 b
    -- describe "buildHeap" $ do
    --     it "builds a heap from an empty list" $ do
    --         let heap = buildHeap ([] :: [Int])
    --         heap `shouldBe` BinLeaf

    --     it "builds a heap from a list with one element" $ do
    --         let heap = buildHeap [1]
    --         heap `shouldBe` BinNode 1 BinLeaf BinLeaf

    --     it "builds a heap from a list of integers" $ do
    --         let heap = buildHeap [4, 3, 2, 1]
    --         heap `shouldBe` BinNode 1 (BinNode 2 BinLeaf BinLeaf) (BinNode 3 (BinNode 4 BinLeaf BinLeaf) BinLeaf)

-- Task7
    describe "toList" $ do
        it "returns a list of elements in the tree" $ do
            let tree = branchSize 
                        (branchSize (leafSize (1 :: Integer)) (leafSize (2 :: Integer)))
                        (branchSize (leafSize (3 :: Integer)) (branchSize (leafSize (4 :: Integer)) (leafSize (5 :: Integer))))
            toList tree `shouldBe` [1, 2, 3, 4, 5]


    describe "tag" $ do
        it "returns the tag of the tree" $ do
            let tree = branchSize 
                        (branchSize (leafSize (1 :: Integer)) (leafSize (2 :: Integer)))
                        (branchSize (leafSize (3 :: Integer)) (branchSize (leafSize (4 :: Integer)) (leafSize (5 :: Integer))))
            tag tree `shouldBe` 5

    describe "head'" $ do
        it "returns the leftmost element of the tree" $ do
            let tree = branchSize 
                        (branchSize (leafSize (1 :: Integer)) (leafSize (2 :: Integer)))
                        (branchSize (leafSize (3 :: Integer)) (branchSize (leafSize (4 :: Integer)) (leafSize (5 :: Integer))))
            head' tree `shouldBe` 1

    describe "getInd" $ do
        it "returns the nth leaf of the tree" $ do
            let tree = branchSize 
                        (branchSize (leafSize (1 :: Integer)) (leafSize (2 :: Integer)))
                        (branchSize (leafSize (3 :: Integer)) (branchSize (leafSize (4 :: Integer)) (leafSize (5 :: Integer))))

            getInd tree 1 `shouldBe` 1
            getInd tree 2 `shouldBe` 2
            getInd tree 3 `shouldBe` 3
            getInd tree 4 `shouldBe` 4
            getInd tree 5 `shouldBe` 5

-- Task8
    describe "getWinner" $ do
        it "returns the element with the highest priority in a branch of two leaves" $ do
            let leaf1 = leafPrio 16 'a'
            let leaf2 = leafPrio 4 'b'
            let branch1 = branchPrio leaf1 leaf2
            getWinner branch1 `shouldBe` 'b'

        it "returns the element with the highest priority in a tree of one branch and one leaf" $ do
            let leaf1 = leafPrio 16 'a'
            let leaf2 = leafPrio 4 'b'
            let branch1 = branchPrio leaf1 leaf2
            let leaf3 = leafPrio 2 'c'
            let branch2 = branchPrio branch1 leaf3
            getWinner branch2 `shouldBe` 'c'

        it "returns the element with the highest priority in a larger tree" $ do
            let leaf1 = leafPrio 16 'a'
            let leaf2 = leafPrio 4 'b'
            let branch1 = branchPrio leaf1 leaf2
            let leaf3 = leafPrio 2 'c'
            let branch2 = branchPrio branch1 leaf3
            let leaf4 = leafPrio 32 'd'
            let leaf5 = leafPrio 8 'e'
            let branch3 = branchPrio leaf4 leaf5
            let root = branchPrio branch2 branch3
            getWinner root `shouldBe` 'c'