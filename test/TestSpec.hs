module TestSpec where

import Test.Hspec
import MyLib

-- Some predefined lists
emptyIntList :: [Int]
emptyIntList = []

list123 :: [Int]
list123 = [1, 2, 3]

spec :: Spec
spec = do

-- Task 2
--
  describe "or'" $ do
    it "all elements are True" $ do
      or' [True, True, True] `shouldBe` True

    it "one element is True" $ do
      or' [False, True, False] `shouldBe` True

    it "all elements are False" $ do
      or' [False, False, False] `shouldBe` False

    it "empty list" $ do
      or' [] `shouldBe` False

  describe "length'" $ do
    it "empty list" $ do
      length' [] `shouldBe` 0

    it "list with multiple elements" $ do
      length' list123 `shouldBe` 3

    it "long list" $ do
      length' ([1..1000000] :: [Int]) `shouldBe` 1000000

  describe "maximum'" $ do
    it "empty list" $ do
      maximum' emptyIntList `shouldBe` Nothing
    
    it "list with one value" $ do
      maximum' ([1] :: [Int]) `shouldBe` Just 1

    it "list with multiple values" $ do
      maximum' (list123 ++ list123) `shouldBe` Just 3

    it "long list" $ do
      maximum' ([1, 3..100000] :: [Int]) `shouldBe` Just 99999

  describe "reverse'" $ do
    it "empty list" $ do
      reverse' emptyIntList `shouldBe` emptyIntList

    it "list with one value" $ do
      reverse' ([1] :: [Int]) `shouldBe` ([1] :: [Int])

    it "list with multiple values" $ do
      reverse' list123 `shouldBe` ([3, 2, 1] :: [Int])

    it "long list" $ do
      reverse' ([1..100000] :: [Int]) `shouldBe` (reverse [1..100000] :: [Int])

  describe "filter'" $ do
    it "empty list" $ do
      filter' (>5) emptyIntList `shouldBe` emptyIntList

    it "list with multiple values 1" $ do
      filter' (>5) list123 `shouldBe` []

    it "list with multiple values 2" $ do
      filter' odd ([1, 2, 3, 4, 5] :: [Int]) `shouldBe` ([1, 3, 5] :: [Int])

    it "long list" $ do
      filter' (>100) ([1..100000] :: [Int]) `shouldBe` ([101..100000] :: [Int])

  describe "map'" $ do
    it "empty list" $ do
      map' succ emptyIntList `shouldBe` emptyIntList

    it "list with multiple values" $ do
      map' succ list123 `shouldBe` ([2, 3, 4] :: [Int])

    it "long list" $ do
      map' pred ([1..100000] :: [Int]) `shouldBe` ([0..99999] :: [Int])

  describe "head'" $ do
    it "empty list" $ do
      head' emptyIntList `shouldBe` Nothing

    it "list with multiple values" $ do
      head' list123 `shouldBe` Just 1

    it "long list" $ do
      head' ([1..100000] :: [Int]) `shouldBe` Just 1

  describe "last'" $ do
    it "empty list" $ do
      last' emptyIntList `shouldBe` Nothing

    it "list with multiple values" $ do
      last' list123 `shouldBe` Just 3

    it "long list" $ do
      last' ([1..100000] :: [Int]) `shouldBe` Just 100000

-- Task 3
  describe "quicksort" $ do
    it "empty list" $ do
      quicksort emptyIntList `shouldBe` emptyIntList

    it "list with multiple values" $ do
      quicksort ([3, -1, 2, 0, 4, 1] :: [Int]) `shouldBe` ([-1, 0, 1, 2, 3, 4] :: [Int])

  describe "insert" $ do
    it "empty list" $ do
      insert emptyIntList 1 `shouldBe` ([1] :: [Int])

    it "insert in the middle" $ do
      insert list123 2 `shouldBe` ([1, 2, 2, 3] :: [Int])

    it "insert in the beginning" $ do
      insert list123 (-1) `shouldBe` ([-1, 1, 2, 3] :: [Int])

  describe "insertionSort" $ do
    it "empty list" $ do
      insertionSort emptyIntList `shouldBe` emptyIntList

    it "list with multiple values" $ do
      insertionSort ([3, -1, 2, 0, 4, 1] :: [Int]) `shouldBe` ([-1, 0, 1, 2, 3, 4] :: [Int])

    it "another list" $ do
      insertionSort (reverse [1..10000] :: [Int]) `shouldBe` ([1..10000] :: [Int])

-- Task 4
  describe "myZipWith" $ do
    it "empty list" $ do
      myZipWith (+) emptyIntList emptyIntList `shouldBe` emptyIntList

    it "list with multiple values + empty list" $ do
      myZipWith (+) list123 emptyIntList `shouldBe` emptyIntList

    it "list with multiple values + list with multiple values" $ do
      myZipWith (+) list123 list123 `shouldBe` ([2, 4, 6] :: [Int])

    it "lists with strings" $ do
      myZipWith (++) ["a", "b", "c"] ["a"] `shouldBe` (["aa"] :: [String])

  describe "fibs" $ do
    it "take n elements from infinite list" $ do
      take 6 fibs `shouldBe` ([0, 1, 1, 2, 3, 5] :: [Int]) 

  describe "fibSum" $ do
    it "empty list" $ do
      fibSum emptyIntList `shouldBe` 0
    it "list 123" $ do
      fibSum list123 `shouldBe` 6
    it "another list" $ do
      fibSum ([0, 0, 0, 1, 0, 1] :: [Int]) `shouldBe` 2

  describe "bfs" $ do
    it "tree1: tree with values of type Int" $ do
     let tree1 = Node 0 [Leaf, Node 1 [Leaf, Node 3 [Leaf]], Node 2 [Node 4 [Leaf]]]
      in bfs tree1 `shouldBe` ([0, 1, 2, 3, 4] :: [Int])
--
--         0
--     /   |   \  
--  Leaf   1    2
--       /   \    \
--    Leaf     3    4
--             |    |
--           Leaf  Leaf


  it "tree2: bamboo" $ do
     let tree2 = Node 0 [Node 1 [Node 2 [Leaf]]]
      in bfs tree2 `shouldBe` ([0, 1, 2] :: [Int])
--
--             0
--             |
--             1
--             |
--             2
--             |
--            Leaf
        
  it "tree3: tree with values of type String" $ do
     let tree3 = Node "Haskell" [Node "language" [Node "is" [Leaf], Node "the" [Node "!" [Leaf], Leaf], Node "best" [Leaf]]]
      in bfs tree3 `shouldBe` (["Haskell", "language", "is", "the", "best", "!"] :: [String])
--
--             'Haskell'
--                 |
--             'language'
--          /      |       \
--       'is'     'the'    'best'
--         |      /  \
--       Leaf    '!'  Leaf

  it "tree4: empty tree == Leaf" $ do
    let tree4 = Leaf
      in bfs tree4 `shouldBe` emptyIntList
  
  describe "dfs" $ do
    it "tree1: tree with values of type Int" $ do
     let tree1 = Node 0 [Leaf, Node 1 [Leaf, Node 3 [Leaf]], Node 2 [Node 4 [Leaf]]]
      in dfs tree1 `shouldBe` ([0, 1, 3, 2, 4] :: [Int])
--
--         0
--     /   |   \  
--  Leaf   1    2
--       /   \    \
--    Leaf     3    4
--             |    |
--           Leaf  Leaf

  it "tree2: bamboo" $ do
     let tree2 = Node 0 [Node 1 [Node 2 [Leaf]]]
      in dfs tree2 `shouldBe` ([0, 1, 2] :: [Int])
--
--             0
--             |
--             1
--             |
--             2
--             |
--            Leaf

  it "tree3: tree with values of type String" $ do
     let tree3 = Node "Haskell" [Node "language" [Node "is" [Leaf], Node "the" [Node "!" [Leaf], Leaf], Node "best" [Leaf]]]
      in dfs tree3 `shouldBe` (["Haskell", "language", "is", "the", "!", "best"] :: [String])
--
--             'Haskell'
--                 |
--             'language'
--          /      |       \
--       'is'     'the'    'best'
--         |      /  \
--       Leaf    '!'  Leaf

  it "tree4: empty tree == Leaf" $ do
    let tree4 = Leaf
      in dfs tree4 `shouldBe` emptyIntList