{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module TestSpec where

import           Test.Hspec
import           MyLib
import qualified Data.Map.Strict as M
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "1 - Arrow" $ do
        it "getArrow" $ do
            getArrow (Arrow (+ (2 :: Int))) 5    `shouldBe` (7 :: Int)
            getArrow (Arrow (< (5 :: Int))) 6    `shouldBe` False
            getArrow (Arrow (+((-7) :: Int))) 3  `shouldBe` ((-4) :: Int)
        it "fmap for Arrow" $ do
            getArrow (fmap (+ (5 :: Int)) (Arrow (+ (2 :: Int)))) 4  `shouldBe` (11 :: Int)
            getArrow (fmap (++" happy!") (Arrow (++" am"))) "I"      `shouldBe` "I am happy!"
            getArrow (fmap (== (5 :: Int)) (Arrow (+ (2 :: Int)))) 4 `shouldBe` False

    let st1 = Student {name = "Lev", grade = 5}
    let st2 = Student {name = "Osa", grade = 4}
    let st3 = Student {name = "Kot", grade = 3}
    let st4 = Student {name = "Kit", grade = 4}
    let st5 = Student {name = "Byk", grade = 2}

    describe "2 - StudentsLog" $ do
        it "calculateStudentsLog" $ do
            calculateStudentsLog [] `shouldBe` StudentsLog  { studentNames = [] :: [String]
                                                            , worstGrade  = Nothing
                                                            , bestGrade   = Nothing}
            calculateStudentsLog [st1, st2, st3] `shouldBe` StudentsLog { studentNames = ["Lev", "Osa", "Kot"]
                                                                        , worstGrade  = Just 3
                                                                        , bestGrade   = Just 5} 
            calculateStudentsLog [st2, st3, st4] `shouldBe` StudentsLog { studentNames = ["Osa", "Kot", "Kit"]
                                                                        , worstGrade  = Just 3
                                                                        , bestGrade   = Just 4}
            calculateStudentsLog [st3, st4, st5] `shouldBe` StudentsLog { studentNames = ["Kot", "Kit", "Byk"]
                                                                        , worstGrade  = Just 2
                                                                        , bestGrade   = Just 4}
        it "calculateStudentsLog'" $ do
            calculateStudentsLog' [] `shouldBe` StudentsLog { studentNames = [] :: [String]
                                                            , worstGrade  = Nothing
                                                            , bestGrade   = Nothing}
            calculateStudentsLog' [st1, st2, st3] `shouldBe` StudentsLog { studentNames = ["Lev", "Osa", "Kot"]
                                                                         , worstGrade  = Just 3
                                                                         , bestGrade   = Just 5} 
            calculateStudentsLog' [st2, st3, st4] `shouldBe` StudentsLog { studentNames = ["Osa", "Kot", "Kit"]
                                                                         , worstGrade  = Just 3
                                                                         , bestGrade   = Just 4}
            calculateStudentsLog' [st3, st4, st5] `shouldBe` StudentsLog { studentNames = ["Kot", "Kit", "Byk"]
                                                                         , worstGrade  = Just 2
                                                                         , bestGrade   = Just 4}

    let a1 = Apple {color = "red", weight = 5.1}
    let a2 = Apple {color = "red", weight = 15.2}
    let a3 = Apple {color = "red", weight = 2.0}
    let a4 = Apple {color = "green", weight = 5.1}
    let a5 = Apple {color = "green", weight = 2.7}
    let a6 = Apple {color = "green", weight = 13.1}
    let a7 = Apple {color = "black", weight = 1.8}
    let a8 = Apple {color = "black", weight = 2.4}
    let tree1 = Node a1 [Node a2 [Node a5 [Leaf]], Node a3 [Leaf], Node a4 [Node a6 [Leaf]]]
    let tree2 = Node a8 [Node a7 [Leaf], Node a6 [Leaf], Node a4 [Node a6 [Leaf]]]
    let tree3 = Node a7 [Leaf]
    let tree4 = Leaf
    let list = ["red", "blue", "green"]

    describe "4 - Apple and Foldable" $ do
        it "applesInRange" $ do
            applesInRange tree1 (5.0, 17.0) `shouldBe` False 
            applesInRange tree2 (1.0, 17.0) `shouldBe` True
            applesInRange tree3 (5.0, 17.0) `shouldBe` False
            applesInRange tree4 (5.0, 17.0) `shouldBe` True
        it "heaviestApple" $ do
            heaviestApple tree1 `shouldBe` Just (Apple {color = "red", weight = 15.2})
            heaviestApple tree3 `shouldBe` Just (Apple {color = "black", weight = 1.8})
            heaviestApple tree4 `shouldBe` Nothing
        it "thisApple" $ do
            thisApple tree1 list (5, 7) `shouldBe` Just (Apple {color = "red", weight = 5.1})
            thisApple tree3 list (5, 15) `shouldBe` Nothing
            thisApple tree4 list (0, 150) `shouldBe` Nothing
        it "sumOfApples" $ do
            sumOfApples tree1 `shouldBe` (43.2 :: Float)
            sumOfApples tree2 `shouldBe` (35.5 :: Float)
            sumOfApples tree3 `shouldBe`  (1.8 :: Float)
            sumOfApples tree4 `shouldBe`  (0.0 :: Float)

    let basket1 = Basket { apples = M.fromList [ ("green",[a5, a4, a6]), ("red",[a3, a1, a2]) ] }
    let basket2 = Basket { apples = M.fromList [ ("black", [a7, a8]), ("green", [a4, a6, a6]) ] }
    let basket3 = Basket { apples = M.fromList [ ("black", [a7]) ] }
    let basket4 = Basket { apples = M.empty }

    describe "5 - Basket" $ do
        it "collectBasket" $ do
            collectBasket tree1 `shouldBe` basket1
            collectBasket tree2 `shouldBe` basket2
            collectBasket tree3 `shouldBe` basket3
            collectBasket tree4 `shouldBe` basket4

    let heap1 = BinNode { val = 5
                , left  = BinNode {val=3, left=BinLeaf, right = BinNode {val=4, left=BinLeaf, right=BinLeaf}} 
                , right = BinNode {val=6, left = BinNode {val=7, left=BinLeaf, right=BinLeaf}, right=BinLeaf}
                } 
    let heap2 = BinNode { val = 1
                , left  = BinNode {val=2, left=BinLeaf, right = BinNode {val=3, left=BinLeaf, right=BinLeaf}} 
                , right = BinNode {val=4, left = BinNode {val=5, left=BinLeaf, right=BinLeaf}, right=BinLeaf}
                } 
    let heap3 = BinNode { val = 10
                , left  = BinNode {val=12, left=BinLeaf, right = BinNode {val=15, left=BinLeaf, right=BinLeaf}} 
                , right = BinNode {val=4, left = BinNode {val=5, left=BinLeaf, right=BinLeaf}, right=BinLeaf}
                }
    let heap4 = BinNode { val = 7
                , left  = BinLeaf 
                , right = BinNode {val=4, left = BinNode {val=5, left=BinLeaf, right=BinLeaf}, right=BinLeaf}
                }
    let correctHeap1 = BinNode { val = 3
                , left  = BinNode {val=4, left=BinLeaf, right = BinNode {val=5, left=BinLeaf, right=BinLeaf}} 
                , right = BinNode {val=6, left = BinNode {val=7, left=BinLeaf, right=BinLeaf}, right=BinLeaf}
                }
    let correctHeap2 = heap2
    let correctHeap3 = BinNode { val = 4
                , left  = BinNode {val=12, left=BinLeaf, right = BinNode {val=15, left=BinLeaf, right=BinLeaf}} 
                , right = BinNode {val=5, left = BinNode {val=10, left=BinLeaf, right=BinLeaf}, right=BinLeaf}
                } 
    let correctHeap4 = BinNode { val = 4
                , left  = BinLeaf 
                , right = BinNode {val=5, left = BinNode {val=7, left=BinLeaf, right=BinLeaf}, right=BinLeaf}
                }
    let smallHeap = BinNode 3 (BinNode 4 BinLeaf BinLeaf) (BinNode 5 BinLeaf BinLeaf)

    describe "6 - Binary Heap" $ do
        it "SiftDown" $ do
            siftDown heap1 `shouldBe` (correctHeap1 :: BinaryHeap Int)
            siftDown heap2 `shouldBe` (correctHeap2 :: BinaryHeap Int)
            siftDown heap3 `shouldBe` (correctHeap3 :: BinaryHeap Int)
            siftDown heap4 `shouldBe` (correctHeap4 :: BinaryHeap Int)
        it "buildHeap" $ do
            buildHeap [1] `shouldBe` (BinNode 1 BinLeaf BinLeaf :: BinaryHeap Int)
            buildHeap [4, 3, 5] `shouldBe` (smallHeap :: BinaryHeap Int)
            buildHeap [5, 4, 3] `shouldBe` (smallHeap :: BinaryHeap Int)
    
    let binTree1 = BBranch 5 (BBranch 4 (BBranch 2 (BBranch 1 (BLeaf 0 "*") (BLeaf 0 "*")) (BLeaf 0 "*")) 
                                        (BBranch 1 (BLeaf 0 "**") (BLeaf 0 "*")))
                             (BBranch 3 (BLeaf 0 "***") (BLeaf 0 "*")) 
    let binTree2 = BBranch "o" (BBranch "oo" (BLeaf "end" 1) (BLeaf "end" 2)) (BLeaf "end" 3)
    let binTree3 = BLeaf "Just one" "end of all"

    let sizeTree = branchSize (leafSize "a") (branchSize (branchSize (leafSize "b") (leafSize "c")) (leafSize "d"))
    
    describe "7 - Binary Tree and Size annotation" $ do
        it "toList" $ do
            toList binTree1 `shouldBe` (["*","*","*","**","*","***","*"] :: [String])
            toList binTree2 `shouldBe` ([1, 2, 3] :: [Int])
            toList binTree3 `shouldBe` (["end of all"] :: [String])
        it "tag" $ do
            tag binTree1 `shouldBe` (5 :: Int)
            tag binTree2 `shouldBe` ("o" :: String)
            tag binTree3 `shouldBe` ("Just one" :: String)
        it "head'" $ do
            head' binTree1 `shouldBe` ("*" :: String)
            head' binTree2 `shouldBe` (1 :: Int)
            head' binTree3 `shouldBe` ("end of all" :: String)
        it "getInd" $ do
            getInd sizeTree 3 `shouldBe` ("c" :: String)
            getInd sizeTree 1 `shouldBe` ("a" :: String)
            evaluate (getInd sizeTree 9) `shouldThrow` errorCall "Incorrect number of list!" 
    
    let prioTree1 = branchPrio (leafPrio 5 "a") (branchPrio (branchPrio (leafPrio 11 "b") (leafPrio 3 "c")) (leafPrio 7 "d"))
    let prioTree2 = branchPrio (leafPrio 5 "winner") 
                               (branchPrio (branchPrio (leafPrio 11 "-_-") (leafPrio 3 "|_|")) (leafPrio 7 "..."))
    let prioTree3 = leafPrio 300 "bad joke"

    describe "8 - Priority Tree" $ do
        it "getWinner" $ do
            getWinner prioTree1 `shouldBe` ("c" :: String)
            getWinner prioTree2 `shouldBe` ("|_|" :: String)
            getWinner prioTree3 `shouldBe` ("bad joke" :: String)
    
    describe "9 - модоиды и прочее" $ do
        it "тест на моноид для MySize" $ do
            getMySize (mempty :: MySize)     `shouldBe` (0 :: Int)
            getMySize (MySize 5)             `shouldBe` (5 :: Int)
            getMySize (MySize 5 <> MySize 3) `shouldBe` (8 :: Int)
        it "тест на моноид для MyPriority" $ do
            getMyPriority (mempty :: MyPriority)          `shouldBe` (maxBound :: Int)
            getMyPriority (MyPriority 7)                  `shouldBe` (7 :: Int)
            getMyPriority (MyPriority 8 <> MyPriority 15) `shouldBe` (8 :: Int)
            getMyPriority (MyPriority 8 <> MyPriority 1)  `shouldBe` (1 :: Int)
        it "тест для measure" $ do
            getMySize     (measure (5   :: Int)  :: MySize)     `shouldBe` (1  :: Int)
            getMyPriority (measure (5   :: Int)  :: MyPriority) `shouldBe` (5  :: Int)
            getMyPriority (measure ('5' :: Char) :: MyPriority) `shouldBe` (53 :: Int)

        let leaf1 = leaf 5 :: BinaryTree MyPriority Int
        let leaf2 = leaf 7 :: BinaryTree MyPriority Int
        let leaf3 = leaf 5 :: BinaryTree MySize Int
        let leaf4 = leaf 7 :: BinaryTree MySize Int
        it "create tree" $ do
            leaf3 `shouldBe` (BLeaf (MySize {getMySize = 1}) 5 :: BinaryTree MySize Int)
            leaf1 `shouldBe` (BLeaf (MyPriority {getMyPriority = 5}) 5 :: BinaryTree MyPriority Int)
            branch leaf1 leaf2 `shouldBe` 
                (BBranch (MyPriority {getMyPriority = 5}) 
                    (BLeaf (MyPriority {getMyPriority = 5}) 5)
                    (BLeaf (MyPriority {getMyPriority = 7}) 7) :: BinaryTree MyPriority Int)
            branch leaf3 leaf4 `shouldBe` 
                (BBranch (MySize {getMySize = 2}) 
                    (BLeaf (MySize {getMySize = 1}) 5) 
                    (BLeaf (MySize {getMySize = 1}) 7) :: BinaryTree MySize Int)
