module TestSpec where

import qualified Data.Map.Strict as M
import Data.Monoid
import MyLib
import Test.Hspec
import Prelude hiding (head)

spec :: Spec
spec = do
    -- Task 2
    --
    describe "calculateStudentsLog" $ do
        it "empty list of students" $ do
            let students = []
            let res = calculateStudentsLog students
            studentNames res `shouldBe` []
            worstGrade res `shouldBe` Nothing
            bestGrade res `shouldBe` Nothing

        it "one student" $ do
            let students = [Student "Haskell learner" 5]
            let res = calculateStudentsLog students
            studentNames res `shouldBe` ["Haskell learner"]
            worstGrade res `shouldBe` Just 5
            bestGrade res `shouldBe` Just 5

        it "multiple students" $ do
            let students = [Student "Julia" 4, Student "Bob" 3, Student "Alisa" 5, Student "Julia" 5]
            let res = calculateStudentsLog students
            studentNames res `shouldBe` ["Julia", "Bob", "Alisa", "Julia"]
            worstGrade res `shouldBe` Just 3
            bestGrade res `shouldBe` Just 5

    describe "calculateStudentsLog'" $ do
        it "empty list of students" $ do
            let students = []
            let res = calculateStudentsLog' students
            studentNames res `shouldBe` []
            worstGrade res `shouldBe` Nothing
            bestGrade res `shouldBe` Nothing

        it "one student" $ do
            let students = [Student "Haskell learner" 5]
            let res = calculateStudentsLog' students
            studentNames res `shouldBe` ["Haskell learner"]
            worstGrade res `shouldBe` Just 5
            bestGrade res `shouldBe` Just 5

        it "multiple students" $ do
            let students = [Student "Julia" 4, Student "Bob" 3, Student "Alisa" 5, Student "Julia" 5]
            let res = calculateStudentsLog' students
            studentNames res `shouldBe` ["Julia", "Bob", "Alisa", "Julia"]
            worstGrade res `shouldBe` Just 3
            bestGrade res `shouldBe` Just 5

    -- Task 3
    --
    describe "instance Foldable Tree" $ do
        it "leaf" $ do
            foldMap Sum Leaf `shouldBe` (Sum 0 :: Sum Int)

        it "only one node" $ do
            getSum (foldMap Sum (Node 1 [])) `shouldBe` (1 :: Int)

        it "tree" $ do
            let tree = Node 1 [Node 2 [Leaf], Node 3 [], Node 4 [Node 5 [Leaf]], Leaf, Leaf]
            getSum (foldMap Sum tree) `shouldBe` (15 :: Int)

    -- Task 4
    --
    describe "applesInRange" $ do
        it "only one apple" $ do
            applesInRange (Node (Apple "Black" 1.2) []) (0.0, 5.0) `shouldBe` True

        it "in range" $ do
            let apple1 = Apple "Green" 1.2
            let apple2 = Apple "Red" 0.4
            let appleTree = Node apple1 [Node apple2 []]
            applesInRange appleTree (0.4, 1.8) `shouldBe` True

        it "out of range" $ do
            let apple1 = Apple "Green" 1.2
            let apple2 = Apple "Red" 0.4
            let appleTree = Node apple1 [Node apple2 []]
            applesInRange appleTree (0.1, 0.2) `shouldBe` False

    describe "heaviestApple" $ do
        it "Leaf" $ do
            heaviestApple Leaf `shouldBe` Nothing

        it "only one apple" $ do
            let apple1 = Apple "Green" 1.2
            heaviestApple (Node apple1 []) `shouldBe` Just apple1

        it "multiple apples" $ do
            let apple1 = Apple "Green" 1.2
            let apple2 = Apple "Red" 0.4
            let apple3 = Apple "Blue" 3.1
            let appleTree = Node apple1 [Node apple2 [Leaf, Node apple3 []]]
            heaviestApple appleTree `shouldBe` Just apple3

    describe "thisApple" $ do
        it "Leaf" $ do
            thisApple Leaf ["Red"] (1, 2) `shouldBe` Nothing

        it "only one apple and true" $ do
            let apple1 = Apple "Green" 1.2
            let appleTree = Node apple1 []
            thisApple appleTree ["Red", "Green"] (0, 4) `shouldBe` Just apple1

        it "only one apple and false" $ do
            let apple1 = Apple "Green" 1.2
            let appleTree = Node apple1 []
            thisApple appleTree ["Red", "Green"] (0, 1) `shouldBe` Nothing

        it "multiple apples and true" $ do
            let apple1 = Apple "Green" 1.2
            let apple2 = Apple "Red" 0.4
            let apple3 = Apple "Blue" 3.1
            let appleTree = Node apple1 [Node apple2 [Leaf, Node apple3 []]]
            thisApple appleTree ["Blue", "Red", "Green"] (1, 3) `shouldBe` Just apple1

    describe "sumOfApples" $ do
        it "Leaf" $ do
            sumOfApples Leaf `shouldBe` 0.0

        it "only one apple" $ do
            let apple1 = Apple "Green" 1.2
            let appleTree = Node apple1 []
            sumOfApples appleTree `shouldBe` 1.2

        it "multiple apples" $ do
            let apple1 = Apple "Green" 1.2
            let apple2 = Apple "Red" 0.4
            let apple3 = Apple "Blue" 3.1
            let appleTree = Node apple1 [Node apple2 [Leaf, Node apple3 []]]
            sumOfApples appleTree `shouldBe` 4.7

    -- Task 5
    --
    describe "collectBasket" $ do
        it "Leaf" $ do
            collectBasket Leaf `shouldBe` Basket (M.empty :: M.Map String [Apple])

        it "only one apple" $ do
            let apple1 = Apple "Green" 1.2
            let appleTree = Node apple1 []
            let res = Basket $ M.fromList [("Green", [apple1])]
            collectBasket appleTree `shouldBe` res

        it "multiple apples" $ do
            let apple1 = Apple "Green" 1.2
            let apple2 = Apple "Red" 0.4
            let apple3 = Apple "Blue" 3.1
            let apple4 = Apple "Green" 0.44
            let apple5 = Apple "Red" 0.2
            let appleTree = Node apple1 [Node apple2 [Leaf, Node apple3 [Node apple4 []]], Node apple5 []]
            let res = Basket $ M.fromList [("Green", [apple4, apple1]), ("Red", [apple5, apple2]), ("Blue", [apple3])]
            collectBasket appleTree `shouldBe` res

    -- Task 6
    --
    describe "siftDown" $ do
        let heap1 = BinNode 1 (BinNode 2 BinLeaf BinLeaf) (BinNode 3 BinLeaf BinLeaf) :: BinaryHeap Int
        let heap2 = BinLeaf :: BinaryHeap Int
        --
        --            1
        --           / \
        --          2   3
        --
        let leftTree = BinNode 3 (BinNode 4 BinLeaf BinLeaf) (BinNode 5 BinLeaf BinLeaf)
        let rightTree = BinNode 4 (BinNode 7 BinLeaf BinLeaf) (BinNode 8 BinLeaf BinLeaf)
        let heap3 = BinNode 6 leftTree rightTree :: BinaryHeap Int

        let resLeftTree = BinNode 4 (BinNode 6 BinLeaf BinLeaf) (BinNode 5 BinLeaf BinLeaf)
        let resRightTree = BinNode 4 (BinNode 7 BinLeaf BinLeaf) (BinNode 8 BinLeaf BinLeaf)
        let resHeap3 = BinNode 3 resLeftTree resRightTree :: BinaryHeap Int
        --
        --            6                        3
        --          /   \                    /   \
        --         3     4           ->     4     4
        --        / \   / \                / \   / \
        --       4   5 7   8              6   5 7   8
        --
        it "empty heap" $ do
            siftDown heap2 `shouldBe` heap2

        it "correct heap" $ do
            siftDown heap1 `shouldBe` heap1

        it "incorrect heap" $ do
            siftDown heap3 `shouldBe` resHeap3

    -- Task 7
    --
    describe "A list with random access" $ do
        let leaf = BLeaf 1 "a" :: BinaryTree Int String
        let leftTree = BBranch 1 (BLeaf 1 "h") (BLeaf 1 "e") :: BinaryTree Int String
        let rightTree = BBranch 1 (BLeaf 1 "l") (BBranch 1 (BLeaf 1 "l") (BLeaf 1 "o")) :: BinaryTree Int String
        let tree = BBranch 1 leftTree rightTree :: BinaryTree Int String
        --
        -- tree:
        --           t
        --        /    \
        --       t      t
        --     /  \    /  \
        --    h    e  l    t
        --                /  \
        --               l    o
        --
        -- sizeTree:
        --           5
        --        /     \
        --       2       3
        --     /  \    /  \
        --    a    b  2    d
        --           / \
        --          c   e
        --
        it "toList: just leaf" $ do
            toList leaf `shouldBe` (["a"] :: [String])

        it "toList: tree" $ do
            toList tree `shouldBe` (["h", "e", "l", "l", "o"] :: [String])

        it "tag: just Leaf" $ do
            tag leaf `shouldBe` 1

        it "tag: tree" $ do
            tag tree `shouldBe` 1

        it "head: leaf" $ do
            head leaf `shouldBe` "a"

        it "head: tree" $ do
            head tree `shouldBe` "h"

        it "getInd: leaf" $ do
            let sizeLeaf = leafSize "a" :: BinaryTree Size String
            getInd sizeLeaf 1 `shouldBe` "a"

        it "getInd: tree" $ do
            getInd sizeTree 1 `shouldBe` "a"
            getInd sizeTree 3 `shouldBe` "c"
            getInd sizeTree 5 `shouldBe` "d"