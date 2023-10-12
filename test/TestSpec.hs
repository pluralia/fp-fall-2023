module TestSpec (spec) where

import MyLib
    ( dfs,
      bfs,
      fibs,
      myZipWith,
      insertionSort,
      insert,
      quicksort,
      take'',
      take',
      last',
      head',
      map',
      filter',
      reverse',
      maximum',
      length',
      or',
      Tree(Node) )

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  , shouldMatchList
  )

spec :: Spec
spec = do
  describe "test `or'" $ do
    it "Empty list" $ do
      or' [] `shouldBe` (False :: Bool)

    it "True item1" $ do
      or' [True, False] `shouldBe` (True :: Bool)

    it "True item2" $ do
      or' [True, True] `shouldBe` (True :: Bool)

    it "False items" $ do
      or' [False, False] `shouldBe` (False :: Bool)

  describe "test `length'" $ do
    it "Empty list" $ do
      length' [] `shouldBe` (0 :: Int)

    it "1 element" $ do
      length' [1] `shouldBe` (1 :: Int)

    it "A LOT OF ELEMENTS" $ do
      length' [1..10000] `shouldBe` (10000 :: Int)


  describe "test `maximum'" $ do
    it "Empty list" $ do
      maximum' [] `shouldBe` (Nothing :: Maybe Int)

    it "1 element" $ do
      maximum' [1234] `shouldBe` (Just 1234 :: Maybe Int)

    it "LOOOOOTS OF ELEMENTS" $ do
      maximum' [1, 23, 44, 56, 2323, 2, 77, 44444] `shouldBe` (Just 44444 :: Maybe Int)


  describe "test `reverse'" $ do
    it "Empty list" $ do
      reverse' [] `shouldBe` ([] :: [Int])

    it "1 element" $ do
      reverse' [5] `shouldBe` ([5] :: [Int])

    it "Big numberzzz" $ do
      reverse' [1, 3..10] `shouldBe` ([9, 7, 5, 3, 1] :: [Int])


  describe "test `filter'" $ do
    it ">0" $ do
      filter' (> 0) [10, -3, 33, -1, -22] `shouldBe` ([10, 33] :: [Int])
      -- а можно просто > 0

    it "null at the end" $ do
      filter' null [[1, 1, 1, 1], [], [2, 3], []] `shouldBe` ([[], []] :: [[Int]])

    it "equality to 8" $ do
      filter' (==8) [8, 9, 2, 2, 3, 3, 8, 8, 8, 8] `shouldBe` ([8, 8, 8, 8, 8] :: [[Int]])


  describe "test `map'" $ do
    it "*x" $ do
      map' (*x) [1, 2, 3, 4] `shouldBe` ([1, 4, 9, 16] :: [Int])

    it "Let it SHOW" $ do
      map' show [0, 1, 0, 1] `shouldBe` (["0", "1", "0", "1"] :: [String])


  describe "test head'" $ do
    it "Empty list" $ do
      head' [] `shouldBe` (Nothing :: Maybe Int)

    it "1 element" $ do
      head' [1] `shouldBe` (Just 1 :: Maybe Int)

    it "A pile of things inside" $ do
      head' [22, 33..99] `shouldBe` (Just 22 :: Maybe Int)


  describe "test last'" $ do
    it "Empty list" $ do
     last' [] `shouldBe` (Nothing :: Maybe Int)

    it "1 element" $ do
      last' [234] `shouldBe` (Just 234 :: Maybe Int)

    it "MMMMMAAAAANNNNYYYYY" $ do
      last' [1, 2, 3, 4, 5] `shouldBe` (Just 5 :: Maybe Int)


  describe "test take'" $ do
    it "1 element" $ do
      take' 1 [1] `shouldBe` ([1] :: [Int])

    it "A lot of elements" $ do
      take' 3 [11, 22, 33, 44, 55] `shouldBe` ([11, 22, 33] :: [Int])


  describe "test take''" $ do
    it "1 element" $ do
      take'' 1 [1] `shouldBe` ([1] :: [Int])

    it "many items" $ do
      take'' 3 [11, 22, 33, 44, 55] `shouldBe` ([55, 44, 33] :: [Int])


  describe "test quicksort" $ do
    it "Empty" $ do
        quicksort [] `shouldBe` ([] :: [Int])

    it "1 element sort" $ do
        quicksort [42] `shouldBe` ([42] :: [Int])

    it "Bag of elements" $ do
        quicksort [3, 2, 2, 2, 1] `shouldBe` ([1, 2, 2, 2, 3] :: [Int])

    it "Same elements" $ do
        quicksort [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] `shouldBe` ([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] :: [Int])


  describe "test insert" $ do
    it "Empty" $ do
        insert [] 1 `shouldBe` ([1] :: [Int])

    it "Usual numbers" $ do
        insert [1, 2, 3, 4, 6] 5 `shouldBe` ([1, 2, 3, 4, 5, 6] :: [Int])


  describe "test insertionSort" $ do
    it "Empty" $ do
        insertionSort [] `shouldBe` ([] :: [Int])

    it "1 element" $ do
        insertionSort [1] `shouldBe` ([1] :: [Int])

    it "Normal numbers" $ do
        insertionSort [3, 2, 2, 2, 1] `shouldBe` ([1, 2, 2, 2, 3] :: [Int])

    it "Same elements" $ do
        insertionSort [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] `shouldBe` ([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] :: [Int])


  describe "test myZipWith" $ do
    it "normal +" $ do
        myZipWith (+) [1, 2, 3] [4, 5, 6] `shouldBe` ([5, 7, 9] :: [Int])

    it "normal *" $ do
        myZipWith (*) [1, 2, 3] [4, 5, 6] `shouldBe` ([4, 10, 18] :: [Int])

    it "Defferent +" $ do
        myZipWith (+) [1, 2] [3, 4, 5, 6, 7] `shouldBe` ([4, 6] :: [Int])

    it "Zero *" $ do
        myZipWith (*) [1, 2, 3] [] `shouldBe` ([] :: [Int])


  describe "test fibs" $ do
    it "Waba laba dab dab" $ do
        take 9 fibs `shouldBe` ([0, 1, 1, 2, 3, 5, 8, 13, 21] :: [Integer])


  describe "test bfs" $ do
    it "I am tired of creating what to write here so let`s just say u good because u made it till here" $ do
      let tree = Node 1 [Node 2 [Node 3 []], Node 4 [Node 5 [], Node 6 []]]
      bfs tree `shouldBe` ([1, 2, 4, 3, 5, 6] :: [Int])

    it "AAAAAAAAAAAAAAAAAAA" $ do
      let tree = Node 'A' [Node 'B' [Node 'C' [], Node 'D' []], Node 'E' [Node 'F' []]]
      bfs tree `shouldBe` ("ABECDF" :: [Char])

    it "OP OP OP OP OP OP OP OP OP OP OP OP OP" $ do
      let tree = Node 0 [Node 1 [Node 2 [Node 3 []]], Node 4 [Node 5 []]]
      bfs tree `shouldBe` ([0, 1, 4, 2, 5, 3] :: [Int])

    it "XYZ" $ do
      let tree = Node 'X' [Node 'Y' [], Node 'Z' [Node '!' []]]
      bfs tree `shouldBe` ("XYZ!" :: [Char])

    it "11111111111111111111111" $ do
      let tree = Node 696969696996969696969969696969699696 []
      bfs tree `shouldBe` ([696969696996969696969969696969699696] :: [Integer])


  describe "test dfs" $ do
    it "THE END" $ do
      let tree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
      dfs tree `shouldBe` ([1, 2, 3, 4, 5, 6, 7] :: [Int])

    it "AAAAAAAAAAAAAAAAAAA" $ do
      let tree = Node 'F' [Node 'U' [Node 'N' [], Node 'N' []], Node 'Y' [Node '!' []]]
      bfs tree `shouldBe` ("FUNNY!" :: [Char])

    it "OP OP OP OP OP OP OP OP OP OP OP OP OP" $ do
      let tree = Node 0 [Node 1 [Node 2 [Node 3 []]], Node 4 [Node 5 []]]
      bfs tree `shouldBe` ([0, 1, 2, 3, 4, 5] :: [Int])

    it "XYZ" $ do
      let tree = Node 'X' [Node 'Y' [Node 'W' []], Node 'Z' []]
      bfs tree `shouldBe` ("XYWZ" :: [Char])

    it "11111111111111111111111" $ do
      let tree = Node 696969696996969696969969696969699696 []
      bfs tree `shouldBe` ([696969696996969696969969696969699696] :: [Integer])
