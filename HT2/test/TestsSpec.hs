-- у меня всего 4 hint - 2 здесь и 2 в библиотеке
-- здесь - 2 штуки Warning: Avoid lambda для myZipWith, но я не вижу причин для замены, к тому же тесты не компилятся без этого
module TestsSpec (spec) where

import MyLib

import Test.Hspec
  (
    Spec, it, shouldBe, describe, shouldMatchList
  )

spec :: Spec
spec = do
  describe "test `or'" $ do
    it "empty list" $ do
      or' [] `shouldBe` (False :: Bool)
  
    it "True item" $ do
      or' [True, False, False] `shouldBe` (True :: Bool)

    it "False items" $ do
      or' [False, False] `shouldBe` (False :: Bool)

  describe "test `lenght'" $ do
    it "empty list" $ do
      length' [] `shouldBe` (0 :: Int)
  
    it "1 item" $ do
      length' ['a'] `shouldBe` (1 :: Int)

    -- здесь cabal test поругался на warning: [-Wtype-defaults] но я не вижу для этого причин
    it "Many items" $ do
      length' [1, 3..10] `shouldBe` (5 :: Int)


  describe "test `maximum'" $ do
    it "empty list" $ do
      maximum' [] `shouldBe` (Nothing :: Maybe Int)
  
    it "1 item" $ do
      maximum' [1] `shouldBe` (Just 1 :: Maybe Int)

    it "Many items" $ do
      maximum' [1,5,6,3,4,7,8,9,13,12] `shouldBe` (Just 13 :: Maybe Int)


  describe "test `reverse'" $ do
    it "empty list" $ do
      reverse' [] `shouldBe` ([] :: [Int])
  
    it "Many items" $ do
      reverse' [1, 3..10] `shouldBe` ([9,7,5,3,1] :: [Int])


  describe "test `filter'" $ do
    it "it >5" $ do
      filter' (>5) [1, 3..10] `shouldMatchList` ([7, 9] :: [Int])

    it "odd" $ do
      filter' odd [1, 3..10] `shouldMatchList` ([1,3,5,7,9] :: [Int])

    it "even" $ do
      filter' even [1, 3..10] `shouldMatchList` ([] :: [Int])
    
    
    it "inf" $ do
      take 3 (filter' even [1..]) `shouldMatchList` ([2,4,6] :: [Int])
    


  describe "test `map'" $ do
    it "it *5" $ do
      map' (*5) [1, 3..10] `shouldBe` ([5,15,25,35,45] :: [Int])
  
    
    it "inf" $ do
      take 3 (map' (*5) [1..]) `shouldMatchList` ([5,10,15] :: [Int])
    


  describe "test head'" $ do
    it "empty list" $ do
     head' [] `shouldBe` (Nothing :: Maybe Int)
  
    it "1 item" $ do
      head' [12] `shouldBe` (Just 12 :: Maybe Int)

    it "many items" $ do
      head' [1, 3..10] `shouldBe` (Just 1 :: Maybe Int)


  describe "test last'" $ do
    it "empty list" $ do
     last' [] `shouldBe` (Nothing :: Maybe Int)
  
    it "1 item" $ do
      last' [12] `shouldBe` (Just 12 :: Maybe Int)

    it "many items" $ do
      last' [1, 3..10] `shouldBe` (Just 9 :: Maybe Int)


  describe "test take'" $ do
    it "n > items" $ do
      take' 3 [[1,2,3],[1,4..10]] `shouldBe` ([[1,2,3],[1,4,7,10]] :: [[Int]])

    it "many items" $ do
      take' 3 [1,2,3,4,5] `shouldBe` ([1,2,3] :: [Int])


  describe "test take''" $ do
    it "n > items" $ do
      take'' 3 [[1,2,3],[1,4..10]] `shouldBe` ([[1,2,3],[1,4,7,10]] :: [[Int]])

    it "many items" $ do
      take'' 3 [1,2,3,4,5] `shouldBe` ([3,4,5] :: [Int])


  describe "test quicksort" $ do
    it "case 1" $ do
      quicksort [1,6,3,8,2,9,4,10] `shouldBe` ([1,2,3,4,6,8,9,10] :: [Int])

  describe "test insert" $ do
    it "case 1" $ do
      insert [1, 5..100] 15 `shouldBe` ([1,5,9,13,15,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81,85,89,93,97] :: [Int])

    it "case 2" $ do
      insert [] 12 `shouldBe` ([12] :: [Int])

    it "case 3" $ do
      insert [1, 5..50] 13 `shouldBe` ([1,5,9,13,13,17,21,25,29,33,37,41,45,49] :: [Int])
    
  -- describe 'test insertionSort' $ do ...


  describe "test myZipWith" $ do
    it "case 1" $ do
      myZipWith (+) [1..10] [1, 3..20] `shouldBe` ([2,5,8,11,14,17,20,23,26,29] :: [Int])

    it "case 2" $ do
      myZipWith (\ x y -> x * y) [1, 2, 3] [] `shouldBe` ([] :: [Int])
    
    it "case 3" $ do
      myZipWith (\ x y -> x * y) [1, 2, 3] [3, 4, 5] `shouldBe` ([3, 8, 15] :: [Int])

{-
Warning: Avoid lambda
Found:
  \ x y -> x * y
Perhaps:
  (*)
если честно, не понимаю, чем ему лямбда здесь не нравится
-}


  describe "test fibs" $ do
    it "case 1" $ do
      take 10 fibs `shouldBe` ([0, 1, 1, 2, 3, 5, 8, 13, 21, 34] :: [Integer])


  -- describe "test fibSum" $ do ...


  describe "test bfs" $ do
      it "case 1" $ do
        let tree = Node 'c' 
                    [Node 'o' 
                      [Node 'e' [], Node 'e' []], 
                     Node 'f' 
                      [Node 'i' [], Node 's' []], 
                     Node 'f' 
                      [Node 't' [], Node 'a' [], Node 's' [Node 't' [], Node 'y'[]]]]
        bfs tree `shouldBe` ("coffeeistasty" :: [Char])

      it "case 2" $ do
        let tree = Node 0 [Node 1 [Node 2 [Node 3 []]], Node 4 [Node 5 []]]
        bfs tree `shouldBe` ([0, 1, 4, 2, 5, 3] :: [Int])

      it "case 3" $ do
        let tree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
        bfs tree `shouldBe` ([1, 2, 5, 3, 4, 6, 7] :: [Int])

      it "case 4" $ do
        let tree = Node 'c' 
                    [Node 'o' 
                      [Node 'e' 
                        [Node 'e' []], 
                       Node 'e' []], 
                     Node 'f' 
                      [Node 'i' [], Node 's' [], Node 's' []], 
                     Node 'f' 
                      [Node 't' [Node 't' [], Node 'y'[]], Node 'a' [], Node 's' []]]
        bfs tree `shouldBe` ("coffeeisstasety" :: [Char])


  describe "test dfs" $ do
      it "case 1" $ do
        let tree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
        dfs tree `shouldBe` ([1, 2, 3, 4, 5, 6, 7] :: [Int])

      it "case 2" $ do
        let tree = Node 'A' [Node 'B' [Node 'C' [], Node 'D' []], Node 'E' [Node 'F' []]]
        dfs tree `shouldBe` ("ABCDEF" :: [Char])
      
      it "case 3" $ do
        let tree = Node 'c' 
                    [Node 'o' 
                      [Node 'e' [], Node 'e' []], 
                    Node 'f' [Node 'i' [], Node 's' []], 
                    Node 'f' 
                      [Node 't' [], Node 'a' [], Node 's' [Node 't' [], Node 'y'[]]]]
        dfs tree `shouldBe` ("coeefisftasty" :: [Char])
      
      it "case 4" $ do
        let tree = Node 'c' 
                    [Node 'o' 
                      [Node 'e' 
                        [Node 'e' []], 
                       Node 'e' []], 
                     Node 'f' 
                      [Node 'i' [], Node 's' [], Node 's' []], 
                     Node 'f' 
                      [Node 't' [Node 't' [], Node 'y'[]], Node 'a' [], Node 's' []]]
        bfs tree `shouldBe` ("coffeeisstasety" :: [Char])