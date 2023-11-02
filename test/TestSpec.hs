module TestSpec (spec) where

import MyLib
import Test.Hspec

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Foldable
import Data.List (find)

-- Hlint дает no hints, ворнингов тоже нет

spec :: Spec
spec = do
    describe "Test calculateStudentsLog" $ do
        it "Returns correct values for a list of students" $ do
            let students = [Student "Ivan" 7, Student "Artem" 8, Student "Alexey" 10]
            let log = calculateStudentsLog students
            studentNames log `shouldBe` ["Ivan", "Artem", "Alexey"]
            worstGrade log   `shouldBe` Just 7
            bestGrade log    `shouldBe` Just 10

        it "Returns correct values for an empty list of students" $ do
            let students = []
            let log = calculateStudentsLog students
            studentNames log `shouldBe` []
            worstGrade log   `shouldBe` Nothing
            bestGrade log    `shouldBe` Nothing

        it "Returns correct values for a single student" $ do
            let students = [Student "Hel(-p/-l)" 10000]
            let log = calculateStudentsLog students
            studentNames log `shouldBe` ["Hel(-p/-l)"]
            worstGrade log   `shouldBe` Just 10000
            bestGrade log    `shouldBe` Just 10000


    describe "Test calculateStudentsLog'" $ do
        it "Returns correct values for a list of students" $ do
            let students = [Student "Ivan" 7, Student "Artem" 8, Student "Alexey" 10]
            let log = calculateStudentsLog' students
            studentNames log `shouldBe` ["Ivan", "Artem", "Alexey"]
            worstGrade log   `shouldBe` Just 7
            bestGrade log    `shouldBe` Just 10

        it "Returns correct values for an empty list of students" $ do
            let students = []
            let log = calculateStudentsLog' students
            studentNames log `shouldBe` []
            worstGrade log   `shouldBe` Nothing
            bestGrade log    `shouldBe` Nothing

        it "Returns correct values for a single student" $ do
            let students = [Student "Hel(-p/-l)" 10000]
            let log = calculateStudentsLog' students
            studentNames log `shouldBe` ["Hel(-p/-l)"]
            worstGrade log   `shouldBe` Just 10000
            bestGrade log    `shouldBe` Just 10000


    describe "Test tree Foldable" $ do
        it "FoldMap with leaf" $ do
            foldMap Sum Leaf                                              `shouldBe` (Sum 0  :: Sum Int)

        it "FoldMap with single node" $ do
            foldMap Sum (Node 5 []  )                                     `shouldBe` (Sum 5  :: Sum Int)

        it "FoldMap with complex tree" $ do
            foldMap Sum (Node 1 [Node 2 [Node 3 []], Node 4 [Node 5 []]]) `shouldBe` (Sum 15 :: Sum Int)


    describe "Test applesInRange" $ do
        it "Returns True for a single apple within range" $ do
            applesInRange (Node (Apple "Red" 0.5) []) (0.0, 1.0)    `shouldBe` True

        it "Returns True for all apples within range" $ do
            let apple1 = Apple "Green" 0.8
                apple2 = Apple "Yellow" 0.9
                appleTree = Node apple1 [Node apple2 []]
            applesInRange appleTree (0.5, 1.0)                      `shouldBe` True

        it "Returns False for an apple out of range" $ do
            applesInRange (Node (Apple "Purple" 1.5) []) (0.0, 1.0) `shouldBe` False


    describe "Test heaviestApple" $ do
        it "Empty tree" $ do
            heaviestApple Leaf                        `shouldBe` Nothing

        it "Tree with a single apple" $ do
            heaviestApple (Node (Apple "Red" 0.5) []) `shouldBe` Just (Apple "Red" 0.5)

        it "Tree with multiple apples" $ do
            let apple1 = Apple "Red" 0.5
                apple2 = Apple "Green" 0.7
                apple3 = Apple "Black" 0.9
                tree = Node apple1 [Node apple2 [], Node apple3 []]
            heaviestApple tree                        `shouldBe` Just apple3


    describe "Test thisApple" $ do
        it "Empty tree" $ do
            thisApple Leaf ["Red", "Green"] (0.1, 0.9) `shouldBe` Nothing

        it "No matching apple" $ do
            let apple1 = Apple "Red" 0.5
                apple2 = Apple "Green" 0.7
                apple3 = Apple "Red" 0.9
                tree = Node apple1 [Node apple2 [], Node apple3 []]
            thisApple tree ["Blue"] (0.5, 0.8)         `shouldBe` Nothing

        it "Matching apple" $ do
            let apple1 = Apple "Red" 0.5
                apple2 = Apple "Green" 0.7
                apple3 = Apple "Red" 0.8
                tree = Node apple1 [Node apple2 [], Node apple3 []]
            thisApple tree ["Red"] (0.7, 0.9)          `shouldBe` Just apple3


    describe "Test sumOfApples" $ do
        it "Empty tree" $ do
            sumOfApples Leaf `shouldBe` 0.0

        it "Tree with a single apple" $ do
            let apple1 = Apple "Red" 0.5
                tree = Node apple1 []
            sumOfApples tree `shouldBe` 0.5

        it "Tree with multiple apples" $ do
            let apple1 = Apple "Red" 0.3
                apple2 = Apple "Green" 0.6
                apple3 = Apple "qwertyuiopasdfghjklzxcvbnm" 0.9
                tree = Node apple1 [Node apple2 [], Node apple3 []]
            sumOfApples tree `shouldBe` 1.8


    describe "Test collectBasket" $ do
        it "Empty tree" $ do
            collectBasket Leaf                        `shouldBe` Basket M.empty

        it "Single apple in the tree" $ do
            collectBasket (Node (Apple "Red" 0.5) []) `shouldBe` Basket (M.singleton "Red" [Apple "Red" 0.5])

        it "Multiple apples with the same color" $ do
            let apple1 = Apple "Red" 0.3
                apple2 = Apple "Red" 0.7
                tree = Node apple1 [Node apple2 []]
            collectBasket tree                        `shouldBe` Basket (M.singleton "Red" [apple1, apple2])

        it "Multiple apples with different colors and weights" $ do
            let apple1 = Apple "Red" 0.5
                apple2 = Apple "Green" 0.7
                apple3 = Apple "Red" 0.3
                tree = Node apple1 [Node apple2 [Node apple3 []]]
            collectBasket tree                        `shouldBe` Basket (M.fromList [("Red", [apple1, apple3]), ("Green", [apple2])])


    describe "Test siftDown" $ do
        it "Sifts down one incorrect point" $ do
            let heapBefore = BinNode (8 :: Int) (BinNode (5 :: Int) BinLeaf BinLeaf) (BinNode (3 :: Int) BinLeaf BinLeaf)
                heapAfter  = BinNode (3 :: Int) (BinNode (5 :: Int) BinLeaf BinLeaf) (BinNode (8 :: Int) BinLeaf BinLeaf)
            siftDown heapBefore `shouldBe` heapAfter

        it "Sifts down already correct heap" $ do
            let heapBefore = BinNode (1 :: Int) (BinNode (2 :: Int) BinLeaf BinLeaf) (BinNode (3 :: Int) BinLeaf BinLeaf)
                heapAfter  = BinNode (1 :: Int) (BinNode (2 :: Int) BinLeaf BinLeaf) (BinNode (3 :: Int) BinLeaf BinLeaf)
            siftDown heapBefore `shouldBe` heapAfter


    -- describe "Test buildHeap" $ do
    --     it "Should build a heap from a list of integers" $ do
    --         let input = [7, 6, 8, 10, 9]
    --         let expected = BinNode 6 (BinNode 7 BinLeaf BinLeaf) (BinNode 8 (BinNode 10 BinLeaf BinLeaf) (BinNode 9 BinLeaf BinLeaf))
    --         buildHeap input `shouldBe` expected

    --     it "Should build a heap from a list of characters" $ do
    --         let input = "cabde"
    --         let expected = BinNode 'a' (BinNode 'b' BinLeaf BinLeaf) (BinNode 'c' (BinNode 'd' BinLeaf BinLeaf) (BinNode 'e' BinLeaf BinLeaf))


    describe "Test mytoList" $ do
        it "Returns a list of elements for a leaf" $ do
            mytoList (BLeaf "TagA" 42)                                  `shouldBe` ([42] :: [Int])

        it "Returns a list of elements for a branch" $ do
            mytoList (BBranch "TagB" (BLeaf "TagL" 1) (BLeaf "TagR" 2)) `shouldBe` ([1, 2] :: [Int])


    describe "Test tag" $ do
        it "Returns the tag for a leaf" $ do
            tag (BLeaf ("TagTagTag" :: String) (99 :: Int))                                                                `shouldBe` ("TagTagTag" :: String)

        it "Returns the tag for a branch" $ do
            tag (BBranch ("Tag1" :: String) (BLeaf ("Tag2" :: String) (10 :: Int)) (BLeaf ("Tag3" :: String) (20 :: Int))) `shouldBe` ("Tag1" :: String)
            

    describe "Test head" $ do
        it "Returns the leftmost element of a binary tree" $ do
            myhead (BBranch "root" (BBranch "left" (BLeaf "THIS" 5) (BLeaf "tagg" 8)) (BLeaf "taggg" 12)) `shouldBe` (5 :: Int)

        it "Returns the only element for a tree with a single leaf" $ do
            myhead (BLeaf "tag" "hello")                                                                  `shouldBe` ("hello" :: String)


    describe "Test getInd" $ do
        it "Returns the value of the 1st leaf" $ do
            getInd annotatedTree 1 `shouldBe` 1

        it "Returns the value of the 3rd leaf" $ do
            getInd annotatedTree 3 `shouldBe` 1


    describe "Test getWinner" $ do
        it "Finds the winner in the priority tree" $ do
            let tree = branchPrior (branchPrior (leafPrior 16 'q') (leafPrior 4 'w')) (branchPrior (leafPrior 2 'e') (branchPrior (leafPrior 32 'r') (leafPrior 8 't')))
            getWinner tree `shouldBe` 'e'

