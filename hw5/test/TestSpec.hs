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
        calculateStudentsLog []                                                           `shouldBe` StudentsLog [] Nothing Nothing
        calculateStudentsLog [Student "Alice" 95, Student "Bob" 78, Student "Charlie" 87] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 78) (Just 95)
        calculateStudentsLog [Student "Alice" 90, Student "Bob" 90, Student "Charlie" 90] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 90) (Just 90)

      it "StudentsLog' tests" $ do
        calculateStudentsLog' []                                                           `shouldBe` StudentsLog [] Nothing Nothing
        calculateStudentsLog' [Student "Alice" 95, Student "Bob" 78, Student "Charlie" 87] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 78) (Just 95)
        calculateStudentsLog' [Student "Alice" 90, Student "Bob" 90, Student "Charlie" 90] `shouldBe` StudentsLog ["Alice", "Bob", "Charlie"] (Just 90) (Just 90)

      it "applesInRange tests" $ do
        applesInRange Leaf (0, 5)                                                                                                                      `shouldBe` True
        applesInRange (Node (Apple "Red" 3.0) []) (2.0, 4.0)                                                                                           `shouldBe` True  
        applesInRange (Node (Apple "Green" 1.5) []) (2.0, 4.0)                                                                                         `shouldBe` False
        applesInRange (Node (Apple "Brown" 2.5) [Node (Apple "Yellow" 2.0) [], Node (Apple "Orange" 3.5) [], Node (Apple "Purple" 4.0) []]) (2.0, 4.0) `shouldBe` True

      it "heaviestApple tests" $ do
        heaviestApple Leaf                                                                                   `shouldBe` Nothing
        heaviestApple (Node (Apple "Red" 3.0) [])                                                            `shouldBe` Just (Apple "Red" 3.0)
        heaviestApple (Node (Apple "Red" 0.2) [Node (Apple "Green" 0.15) [], Node (Apple "Yellow" 0.25) []]) `shouldBe` Just (Apple "Yellow" 0.25)

      it "thisApple tests" $ do
        thisApple Leaf ["Red", "Green"] (0.1, 0.3)                                                                                     `shouldBe` Nothing
        thisApple (Node (Apple "Yellow" 0.4) [Node (Apple "Orange" 0.25) [], Node (Apple "Green" 0.15) []]) ["Red", "Blue"] (0.1, 0.3) `shouldBe` Nothing
        thisApple (Node (Apple "Red" 0.2) [Node (Apple "Orange" 0.25) [], Node (Apple "Black" 0.3) []]) ["Red", "Green"] (0.2, 0.3)    `shouldBe` Just (Apple "Red" 0.2)

      it "sumOfApples tests" $ do
        sumOfApples Leaf                                                                                 `shouldBe` 0.0
        sumOfApples (Node (Apple "Red" 0.2) [])                                                          `shouldBe` 0.2
        sumOfApples (Node (Apple "Red" 0.2) [Node (Apple "Green" 0.3) [], Node (Apple "Yellow" 0.5) []]) `shouldBe` 1.0

      it "collectBasket tests" $ do
        collectBasket Leaf  `shouldBe` Basket M.empty
        collectBasket (Node (Apple "Red" 0.2) []) `shouldBe` Basket {apples = M.fromList [("Red",[Apple {color = "Red", weight = 0.2}])]}
        let apple1 = Apple "Red" 0.5
        let apple2 = Apple "Green" 0.3
        let apple3 = Apple "Red" 0.15
        let apple4 = Apple "Green" 0.1
        let apple5 = Apple "Red" 0.22
        let tree1 = Node apple1 [Node apple2 [], Node apple3 []]
        let tree2 = Node apple1 [Node apple2 [], Node apple3 [Node apple4 [], Node apple5 []]]
        collectBasket tree1 `shouldBe` Basket {apples = M.fromList [("Green",[Apple {color = "Green", weight = 0.3}]),("Red",[Apple {color = "Red", weight = 0.15},Apple {color = "Red", weight = 0.5}])]} 
        collectBasket tree2 `shouldBe` Basket {apples = M.fromList [("Green",[Apple {color = "Green", weight = 0.1},Apple {color = "Green", weight = 0.3}]),("Red",[Apple {color = "Red", weight = 0.15},Apple {color = "Red", weight = 0.22},Apple {color = "Red", weight = 0.5}])]}
      
      it "siftDown tests" $ do  
        siftDown (BinNode (42 :: Integer) BinLeaf BinLeaf)
          `shouldBe` BinNode (42 :: Integer) BinLeaf BinLeaf
        siftDown
          (BinNode
              (11 :: Integer) (BinNode (10 :: Integer) BinLeaf BinLeaf)
              (BinNode (8 :: Integer) BinLeaf BinLeaf))
          `shouldBe`
            BinNode
              (8 :: Integer) (BinNode (10 :: Integer) BinLeaf BinLeaf)
              (BinNode (11 :: Integer) BinLeaf BinLeaf)
        let heap
              = BinNode
                  (10 :: Int)
                  (BinNode
                      (91 :: Int) (BinNode (5 :: Int) BinLeaf BinLeaf)
                      (BinNode (5737 :: Int) BinLeaf BinLeaf))
                  (BinNode (16 :: Int) BinLeaf BinLeaf)
        let expected
              = BinNode
                  (10 :: Int)
                  (BinNode
                      (91 :: Int) (BinNode (5 :: Int) BinLeaf BinLeaf)
                      (BinNode (5737 :: Int) BinLeaf BinLeaf))
                  (BinNode (16 :: Int) BinLeaf BinLeaf)
        siftDown heap `shouldBe` expected

      it "toList tests" $ do  
        toList (BLeaf "tag" "a")                                                                                                       `shouldBe` ["a"]
        toList (BBranch "root" (BBranch "left" (BLeaf "l1" "x") (BLeaf "l2" "y")) (BBranch "right" (BLeaf "r1" "z") (BLeaf "r2" "w"))) `shouldBe` ["x", "y", "z", "w"]

      it "tag tests" $ do
        tag (BLeaf "tag" "a")                                                                                                       `shouldBe` "tag"
        tag (BBranch "root" (BBranch "left" (BLeaf "l1" "x") (BLeaf "l2" "y")) (BBranch "right" (BLeaf "r1" "z") (BLeaf "r2" "w"))) `shouldBe` "root"

      it "head' tests" $ do 
        head' (BLeaf "tag" "a")                                                                                                       `shouldBe` "a"
        head' (BBranch "root" (BBranch "left" (BLeaf "l1" "x") (BLeaf "l2" "y")) (BBranch "right" (BLeaf "r1" "z") (BLeaf "r2" "w"))) `shouldBe` "x"
      
      it "head' tests" $ do
        getInd treeWithSizeAnnotations 1 `shouldBe` 'a'
        getInd treeWithSizeAnnotations 2 `shouldBe` 'b'
        getInd treeWithSizeAnnotations 3 `shouldBe` 'c'
        getInd treeWithSizeAnnotations 4 `shouldBe` 'd'
        getInd treeWithSizeAnnotations 5 `shouldBe` 'e'

      it "leafPrio && branchPrio tests" $ do
        leafPrio 2 'a'                               `shouldBe` BLeaf 2 'a'
        branchPrio (leafPrio 4 'a') (leafPrio 2 'b') `shouldBe` BBranch 2 (leafPrio 4 'a') (leafPrio 2 'b')

      it "getWinner tests" $ do
        getWinner (leafPrio 2 'a')                               `shouldBe` 'a'
        getWinner (branchPrio (leafPrio 2 'a') (leafPrio 4 'b')) `shouldBe` 'a'
        getWinner (branchPrio (leafPrio 4 'b') (leafPrio 2 'a')) `shouldBe` 'a'
        getWinner treeWithPriorityAnnotations                    `shouldBe` 'c'

      it "NewSize tests" $ do
        getNewSize mempty                                             `shouldBe` 0
        getNewSize (NewSize (100  :: Int))                            `shouldBe` 100
        getNewSize ((<>) (NewSize (100 :: Int)) (NewSize (1 :: Int))) `shouldBe` 101
        getNewSize (measure (10 :: Int))                              `shouldBe` 1

      it "NewPriority tests" $ do
        getNewPriority mempty                                                      `shouldBe` maxBound
        getNewPriority (NewPriority (100 :: Int))                                  `shouldBe` 100
        getNewPriority ((<>) (NewPriority (100 :: Int)) (NewPriority (1 :: Int)))  `shouldBe` 1
        getNewPriority (measure (10 :: Int))                                       `shouldBe` 10

      it "tree tests" $ do
        let l1 = leaf 2 :: BinaryTree NewPriority Int
            l2 = leaf 4 :: BinaryTree NewSize Int
            l3 = leaf 3 :: BinaryTree NewPriority Int
            l4 = leaf 5 :: BinaryTree NewSize Int

        l1           `shouldBe` (BLeaf (NewPriority {getNewPriority = 2}) 2                                                                                           :: BinaryTree NewPriority Int)
        l2           `shouldBe` (BLeaf (NewSize {getNewSize = 1}) 4                                                                                                   :: BinaryTree NewSize Int)
        branch l1 l3 `shouldBe` (BBranch (NewPriority {getNewPriority = 2}) (BLeaf (NewPriority {getNewPriority = 2}) 2) (BLeaf (NewPriority {getNewPriority = 3}) 3) :: BinaryTree NewPriority Int)
        branch l2 l4 `shouldBe` (BBranch (NewSize {getNewSize = 2}) (BLeaf (NewSize {getNewSize = 1}) 4) (BLeaf (NewSize {getNewSize = 1}) 5)                         :: BinaryTree NewSize Int)
