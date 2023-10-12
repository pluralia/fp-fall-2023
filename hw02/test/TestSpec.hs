module TestSpec where

import Test.Hspec
import MyLib

spec :: Spec
spec = do
  -- 2 задание
  describe "or'" $ do
    it "returns True if at  one of the values in the list is True" $ do
      or' [False, False, True] `shouldBe` True

    it "returns False if all values in the list are False" $ do
      or' [False, False, False] `shouldBe` False

  describe "length'" $ do
    it "returns 0 for the empty list" $ do
      length' [] `shouldBe` 0

    it "returns length for the non-empty list" $ do
      length' ([1..10] :: [Int])  `shouldBe` 10

  describe "maximum'" $ do
    it "returns Nothing for the empty list" $ do
      maximum' [] `shouldBe` Nothing

    it "returns Just max for the non-empty list" $ do
      maximum' ([1..10] :: [Int]) `shouldBe` Just 10

  describe "reverse'" $ do
    it "returns empty list when the input is empty list" $ do
      reverse' ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "returns reversed list for non-empty list" $ do
      reverse' ([1..5] :: [Int]) `shouldBe` ([5,4,3,2,1]:: [Int])

  describe "filter'" $ do
    it "returns empty list when the input is empty list" $ do
      filter' even ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "returns a list with only even elements for non-empty list" $ do
      filter' even ([1..10] :: [Int]) `shouldBe` ([2,4,6,8,10] :: [Int])

    it "returns a list with only elements >=3 for non-empty list" $ do
      filter' (>=3) ([1..10] :: [Int]) `shouldBe` ([3..10] :: [Int])

  describe "map'" $ do
    it "returns empty list when the input is empty list" $ do
      map' (+5) ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "returns a list with each element +5 for a non-empty list" $ do
      map' (+5) ([1..10] :: [Int]) `shouldBe` ([6..15] :: [Int])

  describe "head'" $ do
    it "returns Nothing when the input is empty list" $ do
      head' ([] :: [Int]) `shouldBe` Nothing

    it "returns Just first element for non-empty list" $ do
      head' ([1..10] :: [Int]) `shouldBe` Just 1

  describe "last'" $ do
    it "returns Nothing when the input is empty list" $ do
      last' ([] :: [Int]) `shouldBe` Nothing

    it "returns Just last element for non-empty list" $ do
      last' ([1..10] :: [Int]) `shouldBe` Just 10

  describe "take'" $ do
    it "returns the first n elements of a list" $ do
      take' 3 ([1..5] :: [Int]) `shouldBe` ([1..3] :: [Int])

    it "returns an empty list when n is 0" $ do
      take' 0 ([1..5] :: [Int]) `shouldBe` []

    it "returns the whole list when n is greater than the length of the list" $ do
      take' 10 ([1..5] :: [Int]) `shouldBe` ([1..5] :: [Int])

  describe "take''" $ do
    it "returns the first n elements of a list" $ do
      take'' 3 ([1..5] :: [Int]) `shouldBe` ([1..3] :: [Int])

    it "returns an empty list when n is 0" $ do
      take'' 0 ([1..5] :: [Int]) `shouldBe` ([] :: [Int])

    it "returns the whole list when n is greater than the length of the list" $ do
      take'' 10 ([1..5] :: [Int]) `shouldBe` ([1..5] :: [Int])
-- 3 задание
  describe "quicksort" $ do
    it "returns empty list when the input is empty list" $ do
      quicksort ([] :: [Int]) `shouldBe` ([] :: [Int])

  it "returns sorted list for non-empty list" $ do
    quicksort ([5,1,3,2,4] :: [Int]) `shouldBe` ([1..5] :: [Int])

  it "returns sorted list for non-empty list" $ do
    quicksort ([5,1,3,2,4,2] :: [Int]) `shouldBe` ([1,2,2,3,4,5] :: [Int])

  describe "insert" $ do
    it "returns list with the inserted element when the input an empty list" $ do
      insert ([] :: [Int]) 5 `shouldBe` ([5] :: [Int])

    it "returns sorted list when a new element is inserted" $ do
      insert ([1,2,4,5] :: [Int]) 3 `shouldBe` ([1..5] :: [Int])

  describe "insertionSort" $ do
    it "returns empty list when the input is empty list" $ do
      insertionSort ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "returns sorted list for non-empty list" $ do
      insertionSort ([5,1,3,2,4] :: [Int]) `shouldBe` ([1..5] :: [Int])

    it "returns sorted list for non-empty list with duplicate elements" $ do
      insertionSort ([5,1,3,2,4,2] :: [Int]) `shouldBe` ([1,2,2,3,4,5] :: [Int])
-- 4 задание
  describe "myZipWith" $ do
    it "returns empty list when both input lists are empty" $ do
      myZipWith (+) ([] :: [Int]) ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "returns list with elements combined by the function when two non-empty lists are given" $ do
      myZipWith (+) ([1,2,3] :: [Int]) ([4,5,6] :: [Int]) `shouldBe` ([5,7,9] :: [Int])

    it "returns list with elements combined by the function when two non-empty lists of different lengths are given" $ do
      myZipWith (+) ([1,2,3] :: [Int]) ([4,5] :: [Int]) `shouldBe` ([5,7] :: [Int])

    it "returns list with elements combined by the function when two non-empty lists and a non-commutative function are given" $ do
      myZipWith (-) ([1,2,3] :: [Int]) ([4,5,6] :: [Int]) `shouldBe` ([-3,-3,-3] :: [Int])

  describe "fibs" $ do
    it "returns a list starting with 0 and 1" $ do
      take 2 fibs `shouldBe` [0, 1]

    it "returns a list of fibs" $ do
      take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

  describe "fibSum" $ do
    it "returns 0 when the input list is empty" $ do
      fibSum ([] :: [Int]) `shouldBe` 0

    it "returns the sum of elements at Fibonacci index positions when non-empty list is given" $ do
      fibSum ([1,2,3,4,5,6,7,8] :: [Int]) `shouldBe` 16

    it "returns the sum of elements at Fibonacci index positions when a non-empty list is given" $ do
      fibSum ([-1,-2,-3,-4,-5,-6,-7,-8] :: [Int]) `shouldBe` -16
--6 задание
  describe "bfs" $ do
    it "returns a list of node values in breadth-first order for a single-node tree" $ do
      let tree = Node (1 :: Int) []
      bfs tree `shouldBe` [1]

    it "returns a list of node values in breadth-first order for a two-level tree" $ do
      let tree = Node (1 :: Int) [Node 2 [], Node 3 []]
      bfs tree `shouldBe` [1, 2, 3]

    it "returns a list of node values in breadth-first order for a three-level tree" $ do
      let tree = Node (1 :: Int) [Node 2 [Node 4 [], Node 5 []], Node 3 [Node 6 [], Node 7 []]]
      bfs tree `shouldBe` [1, 2, 3, 4, 5, 6, 7]

  describe "dfs" $ do
      it "returns a list of node values in depth-first order for a single-node tree" $ do
        let tree = Node (1 :: Int) []
        dfs tree `shouldBe` [1]

      it "returns a list of node values in depth-first order for a two-level tree" $ do
        let tree = Node (1 :: Int) [Node 2 [], Node 3 []]
        dfs tree `shouldBe` [1, 2, 3]

      it "returns a list of node values in depth-first order for a three-level tree" $ do
        let tree = Node (1 :: Int) [Node 2 [Node 4 [], Node 5 []], Node 3 [Node 6 [], Node 7 []]]
        dfs tree `shouldBe` [1, 2, 4, 5, 3, 6, 7]


