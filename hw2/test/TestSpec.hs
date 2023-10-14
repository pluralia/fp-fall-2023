module TestSpec (spec) where
    
import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

import MyLib


spec :: Spec
spec = do
  describe "traceFoldl" $ do
    it "returns the correct result" $ do
      traceFoldl (+) 0 [1, 2, 3] `shouldBe` (6 :: Int)
      traceFoldl (*) 1 [1, 2, 3, 4] `shouldBe` (24 :: Int)

  describe "or'" $ do
    it "returns the correct result" $ do
      or' [] `shouldBe` False
      or' [True, False] `shouldBe` True
      or' [False, False, False] `shouldBe` False
      or' [False, True, False] `shouldBe` True
      or' [False, False, False, False, True] `shouldBe` True

  describe "length'" $ do
    it "returns the correct result" $ do
      length' ["1", "2", "3"] `shouldBe` (3 :: Int)
      length' ["1"] `shouldBe` (1 :: Int)
      length' [] `shouldBe` (0 :: Int)
      length' [1..100000] `shouldBe` (100000 :: Int)

  describe "maximum'" $ do
    it "returns the correct result" $ do
      maximum' [1..1000000] `shouldBe` (Just 1000000 :: Maybe Int)
      maximum' [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)
      maximum' [5, 2] `shouldBe` (Just 5 :: Maybe Int)
      maximum' [] `shouldBe` Nothing

  describe "reverse'" $ do
    it "returns the correct result" $ do
      reverse' [1..1000000] `shouldBe` ([1000000, 999999..1] :: [Int])
      reverse' [1, 2, 3] `shouldBe` ([3, 2, 1] :: [Int])
      reverse' [] `shouldBe` ([] :: [Int])

  describe "filter'" $ do
    it "returns the correct result" $ do
      filter' (\x -> x `mod` 100 == 0) [1..1000000] `shouldBe` ([100, 200..1000000] :: [Int])
      filter' even [1, 2, 3, 4, 5] `shouldBe` ([2, 4] :: [Int])
      filter' (> 0) [-1, 0, 1, 2] `shouldBe` ([1, 2] :: [Int])

  describe "map'" $ do
    it "returns the correct result" $ do
      map' (* 2) [1..1000000] `shouldBe` ([2, 4..2000000] :: [Int])
      map' (+ 1) [1, 2, 3] `shouldBe` ([2, 3, 4] :: [Int])
      map' (* 10) [1, 2] `shouldBe` ([10, 20] :: [Int])
      map' (* 10) [] `shouldBe` ([] :: [Int])      

  describe "head'" $ do
    it "returns the correct result" $ do
      head' [1..1000000] `shouldBe` (Just 1 :: Maybe Int)
      head' [1, 2, 3] `shouldBe` (Just 1 :: Maybe Int)
      head' [] `shouldBe` (Nothing :: Maybe [Int])

  describe "last'" $ do
    it "returns the correct result" $ do
      last' [1..1000000] `shouldBe` (Just 1000000 :: Maybe Int)
      last' [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)
      last' [] `shouldBe` (Nothing :: Maybe [Int])

  describe "take'" $ do
    it "returns the correct result" $ do
      take' 100 [1..1000000] `shouldBe` ([1..100] :: [Int])
      take' 2 [1, 2, 3] `shouldBe` ([1, 2] :: [Int])
      take' 5 [1, 2, 3, 4] `shouldBe` ([1, 2, 3, 4] :: [Int])
      take' 2 [] `shouldBe` ([] :: [Int])

  describe "take''" $ do
    it "returns the correct result" $ do
      take'' 100 [1..100000] `shouldBe` ([99901..100000] :: [Int])
      take'' 2 [1, 2, 3] `shouldBe` ([2, 3] :: [Int])
      take'' 5 [1, 2, 3, 4] `shouldBe` ([1, 2, 3, 4] :: [Int])
      take'' 2 [] `shouldBe` ([] :: [Int])

  describe "quicksort" $ do
    it "returns the correct result" $ do
      quicksort [] `shouldBe` ([] :: [Int])
      quicksort [1000, 999..0] `shouldBe` ([0..1000] :: [Int])
      quicksort [3, 2, 4, 1] `shouldBe` ([1, 2, 3, 4] :: [Int])
      quicksort [5, 1, 4, 2, 3] `shouldBe` ([1, 2, 3, 4, 5] :: [Int])

  describe "insert" $ do
    it "returns the correct result" $ do
      insert [1..1000] 1001 `shouldBe` ([1..1001] :: [Int])
      insert [1..1000] 0 `shouldBe` ([0..1000] :: [Int])
      insert [1, 4] 2 `shouldBe` ([1, 2, 4] :: [Int])
      insert [1, 2, 3] 4 `shouldBe` ([1, 2, 3, 4] :: [Int])
      insert [] 4 `shouldBe` ([4] :: [Int])

  describe "insertionSort" $ do
    it "returns the correct result" $ do
      insertionSort [] `shouldBe` ([] :: [Int])
      insertionSort [1000, 999..0] `shouldBe` ([0..1000] :: [Int])
      insertionSort [3, 2, 4, 1] `shouldBe` ([1, 2, 3, 4] :: [Int])
      insertionSort [5, 1, 4, 2, 3] `shouldBe` ([1, 2, 3, 4, 5] :: [Int])

  describe "myZipWith" $ do
    it "returns the correct result" $ do
      myZipWith (++) ["a", "b"] ["c", "d", "e"] `shouldBe` (["ac", "bd"] :: [String])
      myZipWith (*) [1, 2, 3] [4, 5, 6] `shouldBe` ([4, 10, 18] :: [Int])
      myZipWith (+) [1..100000] [1..1000000] `shouldBe` ([2, 4..200000] :: [Int])

  describe "fibs" $ do
    it "returns the correct result" $ do
      take 10 fibs `shouldBe` ([0, 1, 1, 2, 3, 5, 8, 13, 21, 34] :: [Integer])
      take 20 fibs `shouldBe` ([0, 1, 1, 2, 3, 5, 8, 13, 21, 34,
                                55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181] :: [Integer])

  describe "fibSum" $ do
    it "returns the correct result" $ do
      fibSum [0, 0, 0, 0, 4, 0, 6, 7, 0, 9, 10] `shouldBe` (0 :: Int)
      fibSum [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] `shouldBe` (19 :: Int)

  describe "bfs" $ do
    it "returns the correct result" $ do
      let tree0 = Node 1 [Node 2 [Node 5 []], Node 3 [], Node 4 []]
      -- something like this
      --   1
      -- / | \
      -- 2 3 4
      -- |
      -- 5
      bfs tree0 `shouldBe` ([1, 2, 3, 4, 5] :: [Integer])
      let tree1 = Node "1" [Node "2" [Node "3" [Node "4" [Node "5" []]]]]
      -- something like this
      --   1
      --   |
      --   2
      --   |
      --   3
      --   |
      --   4
      --   |
      --   5
      bfs tree1 `shouldBe` (["1", "2", "3", "4", "5"] :: [String])
      let tree2 = Node "dog" [Node "husky" [Node "puppy" [], Node "adult" []], Node "poodle" []]
      -- something like this
      --   dog
      -- /     \
      -- husky  poodle
      -- /   \
      -- puppy adult
      bfs tree2 `shouldBe` (["dog", "husky", "poodle", "puppy", "adult"] :: [String])
      
  describe "dfs" $ do
    it "returns the correct result" $ do
      let tree0 = Node "1" [Node "2" [Node "5" []], Node "3" [], Node "4" []]
      -- look up for ref
      dfs tree0 `shouldBe` (["1", "2", "5", "3", "4"] :: [String])
      let tree1 = Node 1 [Node 2 [Node 3 [Node 4 [Node 5 []]]]]
      --  look up for ref
      dfs tree1 `shouldBe` ([1, 2, 3, 4, 5] :: [Integer])
      let tree2 = Node "dog" [Node "husky" [Node "puppy" [], Node "adult" []], Node "poodle" []]
      -- look up for ref
      dfs tree2 `shouldBe` (["dog", "husky", "puppy", "adult", "poodle"] :: [String])



