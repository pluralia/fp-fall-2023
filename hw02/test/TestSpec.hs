module TestSpec where

 import MyLib

 import Test.Hspec
   (
     Spec
   , it
   , shouldBe
   , describe
   )

 spec :: Spec
 spec = do
   describe "or" $ do
     it "empty list" $ do
       or' [] `shouldBe` (False :: Bool)
     it "false" $ do
       or' [False, False, False] `shouldBe` (False :: Bool)
     it "true" $ do
       or' [True, True, True] `shouldBe` (True :: Bool)
     it "mixed" $ do
       or' [True, False, True] `shouldBe` (True :: Bool)
     it "long list with alternating True and False values" $ do
       or' [True, False, True, False, False, True, False, True] `shouldBe` (True :: Bool)



   describe "length" $ do
     it "empty list" $ do
       length' ([] :: [Int]) `shouldBe` (0 :: Int)
     it "5 elements" $ do
       length' ([1, 2, 3, 4, 5] :: [Int]) `shouldBe` (5 :: Int)
     it "1000" $ do
       length' ([1..1000] :: [Int]) `shouldBe` (1000 :: Int)

   describe "maximum" $ do
     it "empty list" $ do
       maximum' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
     it "max 15" $ do
       maximum' [3, -1, 15] `shouldBe` (Just 15 :: Maybe Int)
     it "max 20" $ do
       maximum' [5, 12, 8, 3, 20, 7] `shouldBe` (Just 20 :: Maybe Int)

   describe "reverse" $ do
     it "empty list" $ do
       reverse' ([] :: [Int]) `shouldBe` ([] :: [Int])
     it "5 elements" $ do
       reverse' [1, 2, 3, 4, 1] `shouldBe` [1, 4, 3, 2, 1 :: Int]

   describe "filter" $ do
     it "empty list" $ do
       filter' (== 1) ([] :: [Int]) `shouldBe` ([] :: [Int])
     it "equals" $ do
       filter' (== 10) [3, 2, 1, 3, 10] `shouldBe` [10 :: Int]
     it "greater" $ do
       filter' (> 2) [3, 2, 1, 3, 10] `shouldBe` [3, 3, 10 :: Int]
     it "less" $ do
       filter' (< 2) [3, 2, 1, 3, 10] `shouldBe` [1 :: Int]

   describe "map" $ do
     it "empty list" $ do
       map' (* 10) ([] :: [Int]) `shouldBe` ([] :: [Int])
     it "add" $ do
       map' (+ 5) [1, 2, 3, 4] `shouldBe` [6, 7, 8, 9 :: Int]
     it "mult" $ do
       map' (* 5) [1, 2, 3, 4] `shouldBe` [5, 10, 15, 20 :: Int]
     it "add big" $ do
       map' (+ 5) [1, 2, 3, 4, 11, 2, 3, 4, 5, 6, 7, 8] `shouldBe` [6, 7, 8, 9, 16, 7, 8, 9, 10, 11, 12, 13 :: Int]
     it "mult big" $ do
       map' (* 2) [1, 2, 3, 4, 11, 2, 3, 4, 5, 6, 7, 8] `shouldBe` [2, 4, 6, 8, 22, 4, 6, 8, 10, 12, 14, 16 :: Int]

   describe "head" $ do
     it "empty list" $ do
       head' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
     it "returns 5" $ do
       head' [-1, 6, 9] `shouldBe` (Just (-1) :: Maybe Int)
     it "returns 1" $ do
       head' [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 2, 3, 4, 5, 6, 500] `shouldBe` (Just 1 :: Maybe Int)

   describe "last" $ do
     it "empty list" $ do
       last' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
     it "returns 9" $ do
       last' [-1, 6, 9] `shouldBe` (Just 9 :: Maybe Int)
     it "returns 500" $ do
       last' [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 2, 3, 4, 5, 6, 500] `shouldBe` (Just 500 :: Maybe Int)

   describe "takeL" $ do
     it "empty list" $ do
       takeL 1 ([] :: [Int]) `shouldBe` ([] :: [Int])
     it "size 2" $ do
       takeL 1 [1, 2] `shouldBe` [1 :: Int]
     it "size 4" $ do
       takeL 2 [4, 1, 1, 4] `shouldBe` [4, 1 :: Int]

   describe "takeR" $ do
     it "empty list" $ do
       takeR 1 ([] :: [Int]) `shouldBe` ([] :: [Int])
     it "size 2" $ do
       takeR 1 [1, 2] `shouldBe` [2 :: Int]
     it "size 4" $ do
       takeR 2 [4, 1, 1, 4] `shouldBe` [1, 4 :: Int]

   describe "quicksort" $ do
     it "empty list" $ do
       quicksort ([] :: [Int]) `shouldBe` ([] :: [Int])
     it "size 1" $ do
       quicksort [0] `shouldBe` [0 :: Int]
     it "size 5" $ do
       quicksort [3, 4, 5, 6, 1] `shouldBe` [1, 3, 4, 5, 6 :: Int]
     it "neg size 5" $ do
       quicksort [-3,-4,-5,-6,-1] `shouldBe` [-6, -5, -4, -3, -1 :: Int]

   describe "insert" $ do
     it "empty list" $ do
       insert ([] :: [Int]) 10 `shouldBe` [10 :: Int]
     it "at start" $ do
       insert [4, 4, 4] 2 `shouldBe` [2, 4, 4, 4 :: Int]
     it "at end" $ do
       insert [4, 4, 4] 5 `shouldBe` [4, 4, 4, 5 :: Int]
     it "at mid" $ do
       insert [2, 2, 4, 4] 3 `shouldBe` [2, 2, 3, 4, 4 :: Int]

   describe "insertionSort" $ do
     it "empty list" $ do
       insertionSort ([] :: [Int]) `shouldBe` ([] :: [Int])
     it "size 1" $ do
       insertionSort [0] `shouldBe` [0 :: Int]
     it "size 5" $ do
       insertionSort [3, 4, 5, 6, 1] `shouldBe` [1, 3, 4, 5, 6 :: Int]
     it "neg size 5" $ do
       insertionSort [-3,-4,-5,-6,-1] `shouldBe` [-6, -5, -4, -3, -1 :: Int]

   describe "myZipWith" $ do
     it "empty list" $ do
       myZipWith (*) ([] :: [Int]) [] `shouldBe` ([] :: [Int])
     it "unbalanced add" $ do
       myZipWith (+) [3, 2, 1] [1, 2] `shouldBe` [4, 4 :: Int]
     it "balanced mult" $ do
       myZipWith (*) [10, 11] [10, 11] `shouldBe` [100, 121 :: Int]

   describe "fibs" $ do
     it "first 6" $ do
       take 6 fibs `shouldBe` [0, 1, 1, 2, 3, 5 :: Integer]
     it "ind 5" $ do
       fibs !! 5 `shouldBe` 5

   describe "test 'fibSum'" $ do
     it "empty list" $ do
       fibSum ([] :: [Integer]) `shouldBe` (0 :: Integer)
     it "size 1" $ do
       fibSum [2] `shouldBe` (2 :: Integer)
     it "size 6" $ do
       fibSum [1,2,3,4,5,6] `shouldBe` (18 :: Integer)

   describe "test 'bfs' and 'dfs' for Tree" $ do
    it "empty tree" $ do
      bfs (Node []) `shouldBe` ([] :: [Int])
      dfs (Node []) `shouldBe` ([] :: [Int])
    it "one node" $ do
      bfs (Node [Leaf 3]) `shouldBe` ([3] :: [Int])
      dfs (Node [Leaf 3]) `shouldBe` ([3] :: [Int])
    it "multiple nodes tree" $ do
      let tree = Node [Leaf 1, Node [Leaf 2, Leaf 3], Leaf 4]
      bfs tree `shouldBe` ([1, 4, 2, 3] :: [Int])
      dfs tree `shouldBe` ([1, 2, 3, 4] :: [Int])
    it "complex tree" $ do
      let complexTree =
            Node [
              Leaf 1,
              Node [
                Leaf 2,
                Node [
                  Leaf 3,
                  Node [Leaf 4, Leaf 5],
                  Leaf 6
                ],
                Leaf 7
              ],
              Node [Leaf 8, Node [Leaf 9, Leaf 10]]
            ]
      bfs complexTree `shouldBe` ([1, 2, 7, 8, 3, 6, 9, 10, 4, 5] :: [Int])
      dfs complexTree `shouldBe` ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Int])
   