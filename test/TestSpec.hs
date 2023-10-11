module TestSpec (spec) where

import MyLib

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
      or [True, False] `shouldBe` (True :: Bool)

    it "True item2" $ do
      or [True, True] `shouldBe` (True :: Bool)

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
    it "Case 1" $ do
      filter' (\x -> x > 0) [10, -3, 33, -1, -22] `shouldMatchList` ([10, 33] :: [Int])
      -- а можно просто > 0

    it "Case 2" $ do
      filter' null [[1, 1, 1, 1], [], [2, 3], []] `shouldMatchList` ([[], []] :: [[Int]])


  describe "test `map'" $ do
    it "Case 1" $ do
      map' (\x -> x * x) [1, 2, 3, 4] `shouldBe` ([1, 4, 9, 16] :: [Int])
      -- а можно просто * x

    it "Case 2" $ do
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
    it "Case 1" $ do
        quicksort [] `shouldBe` ([] :: [Int])

    it "Case 2" $ do
        quicksort [42] `shouldBe` ([42] :: [Int])

    it "Case 3" $ do
        quicksort [3, 2, 2, 2, 1] `shouldBe` ([1, 2, 2, 2, 3] :: [Int])


  describe "test insert" $ do
    it "Case 1" $ do
        insert [] 1 `shouldBe` ([1] :: [Int])

    it "Case 2" $ do
        insert [1, 2, 3, 4, 6] 5 `shouldBe` ([1, 2, 3, 4, 5, 6] :: [Int])


  describe "test insertionSort" $ do
    it "Case 1" $ do
        insertionSort [] `shouldBe` ([] :: [Int])

    it "Case 2" $ do
        insertionSort [1] `shouldBe` ([1] :: [Int])

    it "Case 3" $ do
        insertionSort [3, 2, 2, 2, 1] `shouldBe` ([1, 2, 2, 2, 3] :: [Int])


  describe "test myZipWith" $ do
    it "Case 1" $ do
        myZipWith (+) [1, 2, 3] [4, 5, 6] `shouldBe` ([5, 7, 9] :: [Int])

    it "Case 2" $ do
        myZipWith (*) [1, 2, 3] [4, 5, 6] `shouldBe` ([4, 10, 18] :: [Int])

    it "Case 3" $ do
        myZipWith (\x y -> x + y) [1, 2] [3, 4, 5, 6, 7] `shouldBe` ([4, 6] :: [Int])

    it "Case 4" $ do
        myZipWith (\x y -> x * y) [1, 2, 3] [] `shouldBe` ([] :: [Int])


  describe "test fibs" $ do
    it "Waba laba dab dab" $ do
        take 9 fibs `shouldBe` ([0, 1, 1, 2, 3, 5, 8, 13, 21] :: [Integer])


  describe "test bfs" $ do
    it "I am tired of creating what to write here so let`s just say u good because u made it till here" $ do
      let tree = Node 1 [Node 2 [Node 3 []], Node 4 [Node 5 [], Node 6 []]]
      bfs tree `shouldBe` ([1, 2, 4, 3, 5, 6] :: [Int])


  describe "test dfs" $ do
    it "THE END" $ do
      let tree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
      dfs tree `shouldBe` ([1, 2, 3, 4, 5, 6, 7] :: [Int])
