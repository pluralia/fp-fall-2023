module TestSpec where

import Data.List
import MyLib
import Test.Hspec

-- Some predefined lists
simpleArr :: [Int]
simpleArr = [1, 2, 3]

emptyArr :: [Int]
emptyArr = []

spec :: Spec
spec = do
  -- describe "test 'or'" $ do
  --  it "empty list" $ do
  --    or' [] `shouldBe` (False :: Bool)
  --  it "only false" $ do
  --    or' [False, False, False] `shouldBe` (False :: Bool)
  --  it "with true" $ do
  --    or' [False, True, False] `shouldBe` (True :: Bool)

  describe "task_1" $ do
    it "test_foldl_simple" $ do
      traceFoldl (+) 0 simpleArr `shouldBe` (foldl (+) 0 simpleArr :: Int)

    it "test_foldl_empty" $ do
      traceFoldl (+) 0 emptyArr `shouldBe` (foldl (+) 0 emptyArr :: Int)

    it "test_foldl_simple" $ do
      traceFoldl (+) 0 [1, 2, 3, 4] `shouldNotBe` (sum [1, 2, 3] :: Int)

  describe "task_2" $ do
    it "test_or'_empty" $ do
      or' [] `shouldBe` (False :: Bool)

    it "test_or'_false_false" $ do
      or' [False, False] `shouldBe` (False :: Bool)

    it "test_or'_false_true" $ do
      or' [False, True] `shouldBe` (True :: Bool)

    it "test_or'_true_false" $ do
      or' [True, False] `shouldBe` (True :: Bool)

    it "test_or'_true_true" $ do
      or' [True, True] `shouldBe` (True :: Bool)

    it "test_or'_n_times_1" $ do
      or' (replicate 10 True :: [Bool]) `shouldBe` (True :: Bool)

    it "test_or'_n_times_2" $ do
      or' (replicate 10 False ++ [True] :: [Bool]) `shouldBe` (True :: Bool)

    it "test_or'_n_times_2" $ do
      or' (replicate 10 False :: [Bool]) `shouldBe` (False :: Bool)

    it "test_length'_empty" $ do
      length' [] `shouldBe` (0 :: Int)

    it "test_length'_1" $ do
      length' ([1] :: [Int]) `shouldBe` (1 :: Int)

    it "test_length'_empty" $ do
      length' ([1, 2, 3] :: [Int]) `shouldBe` (3 :: Int)

    it "test_maximum'_empty" $ do
      maximum' [] `shouldBe` (Nothing :: Maybe Int)

    it "test_maximum'_1_2_3" $ do
      maximum' [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)

    it "test_reverse'_-3_-2_-1" $ do
      reverse' [-3, -2, -1] `shouldBe` ([-1, -2, -3] :: [Int])

    it "test_reverse'_empty" $ do
      reverse' [] `shouldBe` ([] :: [Int])

    it "test_filter'_0" $ do
      filter' (> 3) [] `shouldBe` (filter (> 3) [] :: [Int])

    it "test_filter'_1" $ do
      filter' (> 3) [1, 2, 3, 4] `shouldBe` (filter (> 3) [1, 2, 3, 4] :: [Int])

    it "test_map'_0" $ do
      map' (* 2) [] `shouldBe` ([] :: [Int])

    it "test_map'_1" $ do
      map' (* 2) [1, 2, 3] `shouldBe` (map (* 2) [1, 2, 3] :: [Int])

    it "test_head'_0" $ do
      head' [] `shouldBe` (Nothing :: Maybe Int)

    it "test_head'_1" $ do
      head' [1, 2, 3] `shouldBe` (Just 1 :: Maybe Int)

    it "test_last'_0" $ do
      last' [] `shouldBe` (Nothing :: Maybe Int)

    it "test_last'_1" $ do
      last' [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)

    it "test_takeL_0" $ do
      takeL 1 [] `shouldBe` ([] :: [Int])

    it "test_takeL_1" $ do
      takeL 2 [1, 2, 3] `shouldBe` (take 2 [1, 2, 3] :: [Int])

    it "test_takeR_0" $ do
      takeR 1 [] `shouldBe` ([] :: [Int])

    it "test_takeR_1" $ do
      takeR 2 [1, 2, 3] `shouldBe` ([2, 3] :: [Int])

  describe "task_3" $ do
    it "test_quicksort_1" $ do
      quicksort ([] :: [Int]) `shouldBe` sort ([] :: [Int])

    it "test_quicksort_2" $ do
      quicksort ([3, 2, 1] :: [Int]) `shouldBe` sort ([3, 2, 1] :: [Int])

    it "test_quicksort_3" $ do
      quicksort ([1 .. 1000] ++ [1000, 999 .. 1] :: [Int]) `shouldBe` sort ([1 .. 1000] ++ [1 .. 1000] :: [Int])

    it "test_insert_1" $ do
      MyLib.insert ([1, 3, 4] :: [Int]) (2 :: Int) `shouldBe` ([1, 2, 3, 4] :: [Int])

    it "test_insert_2" $ do
      MyLib.insert ([] :: [Int]) (2 :: Int) `shouldBe` ([2] :: [Int])

    it "test_insertionSort_1" $ do
      insertionSort ([] :: [Int]) `shouldBe` sort ([] :: [Int])

    it "test_insertionSort_2" $ do
      insertionSort ([3, 2, 1] :: [Int]) `shouldBe` sort ([3, 2, 1] :: [Int])

  describe "task_5" $ do
    it "test_myZipWith_1" $ do
      myZipWith (+) ([1, 2, 3] :: [Int]) ([1, 2, 3] :: [Int]) `shouldBe` ([2, 4, 6] :: [Int])

    it "test_myZipWith_2" $ do
      myZipWith (+) ([] :: [Int]) ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "test_myZipWith_2" $ do
      myZipWith (+) ([] :: [Int]) ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "test_fibSum_1" $ do
      fibSum ([0, 1, 1, 2, 3, 5, 8, 13, 21] :: [Integer]) `shouldBe` (31 :: Integer)

  describe "task_6" $ do
    it "test_bfs_1" $ do
      let tree1 = Node 0 [Node 1 [Node 2 [Leaf]]]
       in bfs tree1 `shouldBe` ([0, 1, 2] :: [Int])

    it "test_bfs_2" $ do
      let tree1 = Node 0 [Node 1 [Node 3 [Leaf], Node 4 [Leaf]], Node 2 [Node 5 [Leaf]] ]
       in bfs tree1 `shouldBe` ([0, 1, 2, 3, 4, 5] :: [Int])

    it "test_dfs_2" $ do
      let tree1 = Node 0 [Node 1 [Node 3 [Leaf], Node 4 [Leaf]], Node 2 [Node 5 [Leaf]] ]
       in dfs tree1 `shouldBe` ([0, 1, 3, 4, 2, 5] :: [Int])

    it "test_dfs_1" $ do
      let tree1 = Node 0 [Node 1 [Node 2 [Leaf]]]
       in dfs tree1 `shouldBe` ([0, 1, 2] :: [Int])