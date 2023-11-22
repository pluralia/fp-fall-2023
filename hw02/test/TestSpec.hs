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
  describe "test 'or'" $ do
    it "empty list" $ do
      or' [] `shouldBe` (False :: Bool)
    it "only false" $ do
      or' [False, False, False] `shouldBe` (False :: Bool)
    it "with true" $ do
      or' [False, True, False] `shouldBe` (True :: Bool)
    it "very long list" $ do
      or' (replicate 1000000 False ++ [True]) `shouldBe` (True :: Bool)
    it "very long list" $ do
      or' (True : repeat False) `shouldBe` (True :: Bool)

  describe "test 'length'" $ do
    it "empty list" $ do
      length' ([] :: [Int]) `shouldBe` (0 :: Int)
    it "non-empty list" $ do
      length' ([1, 2, 3] :: [Int]) `shouldBe` (3 :: Int)
    it "list with duplicates" $ do
      length' ([1, 1, 1, 1] :: [Int]) `shouldBe` (4 :: Int)
    it "very long list" $ do
      length' (replicate 1000000 10 :: [Int]) `shouldBe` (1000000 :: Int)

  describe "test 'maximum'" $ do
    it "empty list" $ do
      maximum' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
    it "non-empty list" $ do
      maximum' [1, 5, 3] `shouldBe` (Just 5 :: Maybe Int)
    it "list with negative numbers" $ do
      maximum' [-1, -5, -3] `shouldBe` (Just (-1) :: Maybe Int)
    it "very long list" $ do
      maximum' [1..1000000 :: Int] `shouldBe` (Just 1000000 :: Maybe Int)

  describe "test 'reverse'" $ do
    it "empty list" $ do
      reverse' ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "non-empty list" $ do
      reverse' [1, 2, 3] `shouldBe` [3, 2, 1 :: Int]
    it "list with duplicates" $ do
      reverse' [1, 1, 2, 2] `shouldBe` [2, 2, 1, 1 :: Int]
    it "very long list" $ do
      reverse' [1..1000000 :: Int] `shouldBe` [1000000,999999..1]

  describe "test 'filter'" $ do
    it "empty list" $ do
      filter' (== 3) ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "non-empty list with filter" $ do
      filter' (>= 3) [1, 2, 3, 4, 5] `shouldBe` [3, 4, 5 :: Int]
    it "non-empty list without filter" $ do
      filter' (> 5) [1, 2, 3, 4, 5] `shouldBe` ([] :: [Int])
    it "very long list" $ do
      filter' (> 100000) ([1..1000000] :: [Int]) `shouldBe` ([100001..1000000] :: [Int])

  describe "test 'map'" $ do
    it "empty list" $ do
      map' (+ 1) ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "non-empty list" $ do
      map' (* 2) [1, 2, 3] `shouldBe` [2, 4, 6 :: Int]
    it "list with negative numbers" $ do
      map' (\x -> if x < 0 then (-1) * x else x) [-1, 2, -3] `shouldBe` [1, 2, 3 :: Int]
    it "very long list" $ do
      map' (* 2) ([1..1000000] :: [Int]) `shouldBe` ([2, 4..2000000] :: [Int])

  describe "test 'head'" $ do
    it "empty list" $ do
      head' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
    it "non-empty list" $ do
      head' [1, 2, 3] `shouldBe` (Just 1 :: Maybe Int)
    it "list with negative numbers" $ do
      head' [-1, 2, 3] `shouldBe` (Just (-1) :: Maybe Int)
    it "very long list" $ do
      head' ([1..1000000] :: [Int]) `shouldBe` (Just 1 :: Maybe Int)

  describe "test 'last'" $ do
    it "empty list" $ do
      last' ([] :: [Int]) `shouldBe` (Nothing :: Maybe Int)
    it "non-empty list" $ do
      last' [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)
    it "list with negative numbers" $ do
      last' [1, 2, -3] `shouldBe` (Just (-3) :: Maybe Int)
    it "very long list" $ do
      last' ([1..1000000] :: [Int]) `shouldBe` (Just 1000000 :: Maybe Int)

  describe "test 'takeL'" $ do
    it "empty list" $ do
      takeL 2 ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "non-empty list, smaller than n" $ do
      takeL 5 [1, 2, 3] `shouldBe` [1, 2, 3 :: Int]
    it "non-empty list, larger than n" $ do
      takeL 2 [1, 2, 3, 4, 5] `shouldBe` [1, 2 :: Int]
    it "n is negative" $ do
      takeL (-2) [1, 2, 3] `shouldBe` ([] :: [Int])
    it "very long list" $ do
      takeL 100 ([1..1000000] :: [Int]) `shouldBe` [1..100 :: Int]

  describe "test 'takeR'" $ do
    it "empty list" $ do
      takeR 2 ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "non-empty list, smaller than n" $ do
      takeR 5 [1, 2, 3] `shouldBe` [1, 2, 3 :: Int]
    it "non-empty list, larger than n" $ do
      takeR 2 [1, 2, 3, 4, 5] `shouldBe` [4, 5 :: Int]
    it "n is negative" $ do
      takeR (-2) [1, 2, 3] `shouldBe` ([] :: [Int])
    it "very long list" $ do
      takeR 100 ([1..1000000] :: [Int]) `shouldBe` [999901..1000000 :: Int]

  describe "test 'quicksort'" $ do
    it "empty list" $ do
      quicksort ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "single element" $ do
      quicksort [1] `shouldBe` [1 :: Int]
    it "multiple elements" $ do
      quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9 :: Int]
    it "already sorted" $ do
      quicksort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5 :: Int]
    it "reverse sorted" $ do
      quicksort [5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5 :: Int]
    it "very long list" $ do
      quicksort ([10000, 9999..1] :: [Int]) `shouldBe` [1..10000 :: Int]

  describe "test 'insert'" $ do
    it "empty list" $ do
      insert ([] :: [Int]) 1 `shouldBe` [1 :: Int]
    it "insert at the beginning" $ do
      insert [2, 3, 4] 1 `shouldBe` [1, 2, 3, 4 :: Int]
    it "insert at the end" $ do
      insert [1, 2, 3] 4 `shouldBe` [1, 2, 3, 4 :: Int]
    it "insert in the middle" $ do
      insert [1, 3, 4] 2 `shouldBe` [1, 2, 3, 4 :: Int]
    it "insert in a singleton list" $ do
      insert [2] 1 `shouldBe` [1, 2 :: Int]
    it "very long list" $ do
      insert ([1..50000] ++ [50002..100000] :: [Int]) 50001 `shouldBe` [1..100000 :: Int]

  describe "test 'insertionSort'" $ do
    it "empty list" $ do
      insertionSort ([] :: [Int]) `shouldBe` ([] :: [Int])
    it "single element" $ do
      insertionSort [1] `shouldBe` [1 :: Int]
    it "multiple elements" $ do
      insertionSort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9 :: Int]
    it "already sorted" $ do
      insertionSort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5 :: Int]
    it "reverse sorted" $ do
      insertionSort [5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5 :: Int]
    it "very long list" $ do
      insertionSort ([10000, 9999..1] :: [Int]) `shouldBe` [1..10000 :: Int]

  describe "test 'myZipWith'" $ do
    it "empty lists" $ do
      myZipWith (+) ([] :: [Int]) [] `shouldBe` ([] :: [Int])
    it "unequal length lists" $ do
      myZipWith (+) [1, 2, 3] [4, 5] `shouldBe` [5, 7 :: Int]
    it "using multiplication" $ do
      myZipWith (*) [1, 2, 3] [4, 5, 6] `shouldBe` [4, 10, 18 :: Int]
    it "using string concatenation" $ do
      myZipWith (++) ["a", "b", "c"] ["x", "y", "z"] `shouldBe` ["ax", "by", "cz" :: String]
    it "very long list" $ do
      myZipWith (+) (replicate 100000 9 :: [Int]) (replicate 100000 1 :: [Int]) `shouldBe` (replicate 100000 10 :: [Int])

  describe "test 'fibs'" $ do
    it "first 10 elements" $ do
      take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34 :: Integer]
    it "element at index 15" $ do
      fibs !! 15 `shouldBe` 610
    it "element at index 20" $ do
      fibs !! 20 `shouldBe` 6765

  describe "test 'fibSum'" $ do
    it "empty list" $ do
      fibSum ([] :: [Integer]) `shouldBe` (0 :: Integer)
    it "single element list" $ do
      fibSum [10] `shouldBe` (10 :: Integer)
    it "sum at Fibonacci positions" $ do
      fibSum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] `shouldBe` (41 :: Integer)
    it "sum at Fibonacci positions" $ do
      fibSum (replicate 100000 1) `shouldBe` (26 :: Integer)

  describe "test 'bfs' and 'dfs' for Tree" $ do
    it "empty tree" $ do
      bfs (Node (0 :: Int) []) `shouldBe` ([0] :: [Int])
      dfs (Node (0 :: Int) []) `shouldBe` ([0] :: [Int])
    it "single node tree" $ do
      bfs (Node 1 []) `shouldBe` ([1] :: [Int])
      dfs (Node 1 []) `shouldBe` ([1] :: [Int])
    it "multiple nodes tree" $ do
      let tree = Node 1 [Node 2 [Node 5 [], Node 6 []], Node 3 [Node 7 []], Node 4 []]
      bfs tree `shouldBe` ([1, 2, 3, 4, 5, 6, 7] :: [Int])
      dfs tree `shouldBe` ([1, 2, 5, 6, 3, 7, 4] :: [Int])
    it "tree 1" $ do
      let tree_1 = Node 1 [Node 2 [Node 5 [], Node 6 []], Node 3 [Node 7 []], Node 4 [Node 8 [Node 9 []]]]
      bfs tree_1 `shouldBe` ([1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Int])
      dfs tree_1 `shouldBe` ([1, 2, 5, 6, 3, 7, 4, 8, 9] :: [Int])
    it "tree 2" $ do
      let tree_2 = Node 1 [Node 2 [Node 3 [Node 4 [Node 5 [Node 6 [Node 7 [Node 8 [Node 9 []]]]]]]]]
      bfs tree_2 `shouldBe` ([1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Int])
      dfs tree_2 `shouldBe` ([1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Int])
