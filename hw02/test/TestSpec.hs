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
    it "empty list" $ do
      or' [] `shouldBe` (False :: Bool)
  
    it "True item" $ do
      or [True, False] `shouldBe` (True :: Bool)

    it "False items" $ do
      or' [False, False] `shouldBe` (False :: Bool)
--test\TestSpec.hs:21:7-22: Suggestion: Use ||
--Found:
--   or [True, False]
-- Perhaps:
--  (True || False)      
-- тестируем хэндмейд функцию

  describe "test `lenght'" $ do
    it "empty list" $ do
      length' [] `shouldBe` (0 :: Int)
  
    it "1 item" $ do
      length' [1] `shouldBe` (1 :: Int)

    it "Many items" $ do
      length' [1..100] `shouldBe` (100 :: Int)


  describe "test `maximum'" $ do
    it "empty list" $ do
      maximum' [] `shouldBe` (Nothing :: Maybe Int)
  
    it "1 item" $ do
      maximum' [1213] `shouldBe` (Just 1213 :: Maybe Int)

    it "Many items" $ do
      maximum' [1109, 1200, 23423, 354, 39, 2, 1, 459] `shouldBe` (Just 23423 :: Maybe Int)


  describe "test `reverse'" $ do
    it "empty list" $ do
      reverse' [] `shouldBe` ([] :: [Int])
  
    it "1 item" $ do
      reverse' [1213, 943..200] `shouldBe` ([403,673,943,1213] :: [Int])

    it "Many items" $ do
      reverse' [200, 1109..4591] `shouldBe` ([3836,2927,2018,1109,200] :: [Int])


  describe "test `filterl'" $ do
    it "case 2" $ do
      filterl' (\x -> x > 0) [-2, 0, 5, -10, 7] `shouldMatchList` ([5, 7] :: [Int])

    it "case 3" $ do
      filterl' null [[1, 2], [], [3, 4], []] `shouldMatchList` ([[], []] :: [[Int]])

    it "case 4" $ do
      filterl' (\x -> x == 3) [3, 2, 3, 1, 3] `shouldMatchList` ([3, 3, 3] :: [Int])

  describe "test `filterr'" $ do

    it "case 2" $ do
      filterr' (\x -> x > 0) [-2, 0, 5, -10, 7] `shouldMatchList` ([5, 7] :: [Int])

    it "case 3" $ do
      filterr' null [[1, 2], [], [3, 4], []] `shouldMatchList` ([[], []] :: [[Int]])

    it "case 4" $ do
      filterr' (\x -> x == 3) [3, 2, 3, 1, 3] `shouldMatchList` ([3, 3, 3] :: [Int])

  describe "test `map'" $ do
    it "case 1" $ do
      map' (\x -> x * 2) [1, 2, 3, 4, 5] `shouldBe` ([2, 4, 6, 8, 10] :: [Int])
  
    it "case 2" $ do
      map' show [1, 2, 3, 4, 5] `shouldBe` (["1", "2", "3", "4", "5"] :: [String])

--test\TestSpec.hs:73:16-28: Suggestion: Avoid lambda using `infix`
--Found:
--  (\ x -> x > 0)
--Perhaps:
--  (> 0)

--test\TestSpec.hs:79:16-29: Suggestion: Avoid lambda using `infix`
--Found:
--  (\ x -> x == 3)
--Perhaps:
--  (== 3)

--test\TestSpec.hs:83:12-24: Suggestion: Avoid lambda using `infix`
--Found:
--  (\ x -> x * 2)
--Perhaps:
--  (* 2)
-- опять же для теста задаю лямбда функции


  describe "test head'" $ do
    it "empty list" $ do
     head' [] `shouldBe` (Nothing :: Maybe Int)
  
    it "1 item" $ do
      head' [1213] `shouldBe` (Just 1213 :: Maybe Int)

    it "many items" $ do
      head' [1200, 1109..459] `shouldBe` (Just 1200 :: Maybe Int)


  describe "test last'" $ do
    it "empty list" $ do
     last' [] `shouldBe` (Nothing :: Maybe Int)
  
    it "1 item" $ do
      last' [1213] `shouldBe` (Just 1213 :: Maybe Int)

    it "many items" $ do
      last' [1200, 1109..459] `shouldBe` (Just 472 :: Maybe Int)


  describe "test take'" $ do
    it "1 item" $ do
      take' 1 [1213] `shouldBe` ([1213] :: [Int])

    it "many items" $ do
      take' 2 [1200, 1109..459] `shouldBe` ([1200, 1109] :: [Int])


  describe "test take''" $ do
    it "1 item" $ do
      take'' 1 [1213] `shouldBe` ([1213] :: [Int])

    it "many items" $ do
      take'' 2 [1200, 1109..459] `shouldBe` ([563,472] :: [Int])


  describe "test quicksort" $ do
    it "case 1" $ do
      quicksort [3, 4, 1, 9, 2, 6, 3, 5] `shouldBe` ([1,2,3,3,4,5,6,9] :: [Int])

    it "case 2" $ do
      quicksort [42] `shouldBe` ([42] :: [Int])

    it "case 3" $ do
      quicksort [] `shouldBe` ([] :: [Int])

    it "case 4" $ do
      quicksort [5, 5, 5] `shouldBe` ([5, 5, 5] :: [Int])

  describe "test insert" $ do
    it "case 1" $ do
      insert [1, 2, 3, 5, 6] 4 `shouldBe` ([1, 2, 3, 4, 5, 6] :: [Int])

    it "case 2" $ do
      insert [] 4 `shouldBe` ([4] :: [Int])

    it "case 3" $ do
      insert [4] 4 `shouldBe` ([4, 4] :: [Int])


  describe "test insertionSort" $ do
    it "case 1" $ do
      insertionSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5] `shouldBe` ([1,1,2,3,3,4,5,5,5,6,9] :: [Int])

    it "case 2" $ do
      insertionSort [42] `shouldBe` ([42] :: [Int])

    it "case 3" $ do
      insertionSort [] `shouldBe` ([] :: [Int])

    it "case 4" $ do
      insertionSort [5, 5, 5, 5, 5] `shouldBe` ([5, 5, 5, 5, 5] :: [Int])


  describe "test myZipWith" $ do
    it "case 1" $ do
      myZipWith (+) [1, 2, 3] [4, 5, 6] `shouldBe` ([5, 7, 9] :: [Int])

    it "case 2" $ do
      myZipWith (*) [1, 2, 3] [4, 5, 6] `shouldBe` ([4, 10, 18] :: [Int])

    it "case 3" $ do
      myZipWith (\x y -> x * y + 1) [1, 2, 3] [4, 5, 6] `shouldBe` ([5, 11, 19] :: [Int])

    it "case 4" $ do
      myZipWith (\x y -> x + y) [1, 2] [4, 5, 6] `shouldBe` ([5, 7] :: [Int])

    it "case 5" $ do
      myZipWith (\x y -> x * y) [1, 2, 3] [] `shouldBe` ([] :: [Int])


  describe "test fibs" $ do
    it "case 1" $ do
      take 10 fibs `shouldBe` ([0, 1, 1, 2, 3, 5, 8, 13, 21, 34] :: [Integer])

    it "case 2" $ do
      take 5 (drop 5 fibs) `shouldBe` ([5, 8, 13, 21, 34] :: [Integer])


  describe "test fibSum" $ do
    it "case 1" $ do
      fibSum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] `shouldBe` (25 :: Int)

    it "case 2" $ do
      fibSum [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55] `shouldBe` (30 :: Int)


  describe "test bfs" $ do
    it "case 1" $ do
      let tree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
      bfs tree `shouldBe` ([1, 2, 5, 3, 4, 6, 7] :: [Int])

    it "case 2" $ do
      let tree = Node 'A' [Node 'B' [Node 'C' [], Node 'D' []], Node 'E' [Node 'F' []]]
      bfs tree `shouldBe` ("ABECDF" :: [Char])


  describe "test dfs" $ do
    it "case 1" $ do
      let tree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
      dfs tree `shouldBe` ([1, 2, 3, 4, 5, 6, 7] :: [Int])

    it "case 2" $ do
      let tree = Node 'A' [Node 'B' [Node 'C' [], Node 'D' []], Node 'E' [Node 'F' []]]
      dfs tree `shouldBe` ("ABCDEF" :: [Char])
