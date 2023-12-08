{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import Data.Map.Strict qualified as M
import MyLib
import Test.Hspec

spec :: Spec
spec = do
  describe "task_1" $ do
    -- разбирался в твоих тестах
    it "test_1" $ do
      --- getArrow (Arrow (+ 2)) 5
      getArrow (Arrow (+ (2 :: Int))) 5 `shouldBe` (7 :: Int)
      getArrow (Arrow (< (5 :: Int))) 6 `shouldBe` False
      getArrow (Arrow (+ ((-7) :: Int))) 3 `shouldBe` ((-4) :: Int)
    it "test_2" $ do
      getArrow (fmap (+ (5 :: Int)) (Arrow (+ (2 :: Int)))) 4 `shouldBe` (11 :: Int)
      getArrow (fmap (++ " happy!") (Arrow (++ " am"))) "I" `shouldBe` "I am happy!"
      getArrow (fmap (== (5 :: Int)) (Arrow (+ (2 :: Int)))) 4 `shouldBe` False

  describe "task_2" $ do
    let s1 = Student "string1" 1
    let s2 = Student "string2" 2
    let s3 = Student "string3" 3
    let ss1 = []
    let ss2 = [s1, s2, s3]

    it "test_a_1" $ do
      calculateStudentsLog ss1 <==> StudentsLog [] Nothing Nothing `shouldBe` True
    it "test_a_2" $ do
      calculateStudentsLog ss2 <==> StudentsLog ["string1", "string2", "string3"] (Just 1) (Just 3) `shouldBe` True

    it "test_a_1" $ do
      calculateStudentsLog' ss1 <==> StudentsLog [] Nothing Nothing `shouldBe` True
    it "test_a_2" $ do
      calculateStudentsLog' ss2 <==> StudentsLog ["string1", "string2", "string3"] (Just 1) (Just 3) `shouldBe` True

  describe "task_4" $ do
    let apple1 = Apple "r" 1
    let apple2 = Apple "g" 2
    let apple3 = Apple "a" 3
    let tree0 = Leaf
    let tree1 = Node apple3 [Node apple2 [Node apple1 [Node apple2 [Leaf]], Leaf]]

    it "applesInRange_1" $ do
      applesInRange tree1 (0, 4) `shouldBe` True

    it "applesInRange_2" $ do
      applesInRange tree1 (1, 3) `shouldBe` True
    it "applesInRange_3" $ do
      applesInRange tree1 (2, 3) `shouldBe` False
    it "applesInRange_4" $ do
      applesInRange tree0 (2, 3) `shouldBe` True

    it "heaviestApple_1" $ do
      heaviestApple tree0 `shouldBe` Nothing
    it "heaviestApple_2" $ do
      heaviestApple tree1 `shouldBe` Just apple3

    it "thisApple_1" $ do
      thisApple tree1 ["r", "g"] (0, 1) `shouldBe` Just apple1
    it "thisApple_2" $ do
      thisApple tree1 ["g"] (0, 1) `shouldBe` Nothing
    it "thisApple_3" $ do
      thisApple tree0 ["g"] (0, 1) `shouldBe` Nothing
    it "thisApple_4" $ do
      thisApple tree1 ["a"] (0, 4) `shouldBe` Just apple3

    it "sumOfApples_1" $ do
      sumOfApples tree0 `shouldBe` (0 :: Float)
    it "sumOfApples_2" $ do
      sumOfApples tree1 `shouldBe` (8 :: Float)

  describe "task_5" $ do
    let apple1 = Apple "r" 1
    let apple2 = Apple "g" 2
    let apple3 = Apple "a" 3
    let tree0 = Leaf
    let tree1 = Node apple3 [Node apple2 [Node apple1 [Node apple2 [Leaf]], Leaf]]

    let basket1 = Basket {apples = M.empty}
    let basket2 = Basket {apples = M.fromList [("r", [apple1]), ("g", [apple2, apple2]), ("a", [apple3])]}

    it "collectBasket_1" $ do
      collectBasket tree1 `shouldBe` basket2
    it "collectBasket_2" $ do
      collectBasket tree0 `shouldBe` basket1

  describe "task_6" $ do
    
    let heap_broken_1 = (BinNode 4 (BinNode 2 BinLeaf (BinNode 3 BinLeaf BinLeaf)) BinLeaf :: BinaryHeap Integer)
    let heap_fixed_1 = (BinNode 2 (BinNode 3 BinLeaf (BinNode 4 BinLeaf BinLeaf)) BinLeaf :: BinaryHeap Integer)
    let heap_broken_2 = (BinNode 4 (BinNode 3 BinLeaf (BinNode 2 BinLeaf BinLeaf)) BinLeaf :: BinaryHeap Integer)
    let heap_fixed_2 = (BinNode 3 (BinNode 2 BinLeaf (BinNode 4 BinLeaf BinLeaf)) BinLeaf :: BinaryHeap Integer)
    
    it "siftDown_1" $ do
      siftDown heap_broken_1 `shouldBe` heap_fixed_1
    it "siftDown_2" $ do
      siftDown heap_broken_2 `shouldBe` heap_fixed_2
    let build_heap_1 =
          BinNode
            { val = 1,
              left =
                BinNode
                  { val = 3,
                    left =
                      BinNode
                        { val = 4,
                          left = BinLeaf,
                          right = BinLeaf
                        },
                    right = BinLeaf
                  },
              right =
                BinNode
                  { val = 2,
                    left = BinLeaf,
                    right = BinLeaf
                  }
            }
    it "buildHeap_1" $ do
      buildHeap ([4, 3, 2, 1] :: [Integer]) `shouldBe` build_heap_1

    let build_heap_2 =
          BinNode
            { val = 1,
              left =
                BinNode
                  { val = 2,
                    left =
                      BinNode
                        { val = 4,
                          left = BinLeaf,
                          right = BinLeaf
                        },
                    right = BinLeaf
                  },
              right =
                BinNode
                  { val = 3,
                    left = BinLeaf,
                    right = BinLeaf
                  }
            }
    it "buildHeap_2" $ do
      buildHeap ([1, 2, 3, 4] :: [Integer]) `shouldBe` build_heap_2

    describe "task_7" $ do
      let tagTree = BBranch 5 (BBranch 3 (BBranch 2 (BLeaf 1 1) (BLeaf 1 2)) (BLeaf 1 3)) (BBranch 2 (BLeaf 1 4) (BLeaf 1 5))

      it "toList" $ do
        toList tagTree `shouldBe` ([1, 2, 3, 4, 5] :: [Integer])
      it "tag" $ do
        tag tagTree `shouldBe` 5
      it "head'" $ do
        head' tagTree `shouldBe` 1
      it "getInd_1" $ do
        getInd tagTree 1 `shouldBe` 1
      it "getInd_2" $ do
        getInd tagTree 2 `shouldBe` 2
      it "getInd_3" $ do
        getInd tagTree 3 `shouldBe` 3

    describe "task_8" $ do
      it "getWinner" $ do
        getWinner myPriorityTree `shouldBe` (-1)
