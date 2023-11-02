module TestSpec (spec) where

import MyLib

import qualified Data.Map.Strict as M
--import Data.Monoid

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  )

spec :: Spec
spec = do
    it "Task 2. StudentsLog" $ do
        let students1 = [Student "Julia" 7, Student "Olga" 9, Student "Sonya" 10]
        let log1 = calculateStudentsLog students1
        studentNames log1 `shouldBe` (["Julia", "Olga", "Sonya"] ::[String])
        worstGrade log1   `shouldBe` Just 7
        bestGrade log1    `shouldBe` Just 10

        let students2 = []
        let log' = calculateStudentsLog students2
        studentNames log' `shouldBe` []
        worstGrade log'   `shouldBe` Nothing
        bestGrade log'    `shouldBe` Nothing

    it "Task 2. StudentsLog'" $ do
        let students1 = [Student "Julia" 7, Student "Olga" 9, Student "Sonya" 10]
        let log1 = calculateStudentsLog' students1
        studentNames log1 `shouldBe` (["Julia", "Olga", "Sonya"] ::[String])
        worstGrade log1   `shouldBe` Just 7
        bestGrade log1    `shouldBe` Just 10

        let students2 = []
        let log' = calculateStudentsLog' students2
        studentNames log' `shouldBe` []
        worstGrade log'   `shouldBe` Nothing
        bestGrade log'    `shouldBe` Nothing

    it "Task 4. Apples" $ do
        let apple1 = Apple "Green" 0.4
            apple2 = Apple "Yellow" 0.5
            apple3 = Apple "Red" 0.9
            apple4 = Apple "Yellow" 1.5
            appleTree1 = Node apple1 [Node apple2 [], Node apple3 []]
            appleTree2 = Node apple1 [Node apple2 [], Node apple4 []]
        
        applesInRange appleTree1 (0.2, 1.0) `shouldBe` True
        applesInRange appleTree2 (0.2, 1.0) `shouldBe` False

        heaviestApple Leaf `shouldBe` Nothing
        heaviestApple appleTree2 `shouldBe` Just apple4

        thisApple appleTree2 ["Green", "Yellow"] (1.2, 1.6) `shouldBe` Just apple4
        thisApple appleTree1 ["Red", "Yellow"] (0.0, 0.4) `shouldBe` Nothing

        sumOfApples Leaf `shouldBe` 0.0
        sumOfApples appleTree1 `shouldBe` 1.8
        sumOfApples appleTree2 `shouldBe` 2.4

    it "Task 5. Busket" $ do
        collectBasket Leaf `shouldBe` Basket M.empty
        let apple1 = Apple "Green" 0.4
            apple2 = Apple "Yellow" 0.5
            apple3 = Apple "Red" 0.9
            apple4 = Apple "Yellow" 1.5
            appleTree = Node apple1 [Node apple2 [Node apple4 []], Node apple3 []]
        collectBasket appleTree `shouldBe` Basket {apples = M.fromList [("Green",[Apple {color = "Green", weight = 0.4}]),("Red",[Apple {color = "Red", weight = 0.9}]),("Yellow",[Apple {color = "Yellow", weight = 0.5},Apple {color = "Yellow", weight = 1.5}])]}

    it "Task 6. Heap" $ do
        siftDown (BinNode (3 :: Integer) (BinNode (2 :: Integer) BinLeaf BinLeaf) (BinNode (1 :: Integer) BinLeaf BinLeaf)) `shouldBe` BinNode (1 :: Integer) (BinNode (2 :: Integer) BinLeaf BinLeaf) (BinNode (3 :: Integer) BinLeaf BinLeaf)
    
    it "Task 7. A list with random access " $ do
        let tree = BBranch "root" (BBranch "left" (BLeaf "l1" "H") (BLeaf "l2" "S")) (BBranch "right" (BLeaf "r1" "E") (BLeaf "r2" "!"))
        
        toList (BLeaf "tag" "a") `shouldBe` ["a"]
        toList tree `shouldBe` ["H","S","E","!"]

        tag (BLeaf "tag" "a") `shouldBe` "tag"
        tag tree `shouldBe` "root"

        listHead (BLeaf "tag" "a") `shouldBe` "a"
        listHead tree `shouldBe` "H"
        
        getInd annotatedTree 4 `shouldBe` 'I'
        getInd annotatedTree 5 `shouldBe` 'T'
