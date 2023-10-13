
module TestSpec where

import Test.Hspec
import MyLib
spec :: Spec
spec = do

-- Number 2

    describe "or'" $ do
        it "return True if True in list" $ do
            or' [False, True, False] == True
            
        it "return True if True in list" $ do
            or' [False, False, False] == False
            
        it "return True if True in list" $ do
            or' [] == False
    
    describe "length'" $ do
        it "return length of list" $ do
            length' [1, 2, 3] == 3
        
        it "return length of list" $ do
            length' [1] == 1
        
        it "return length of list" $ do
            length' [] == 0
    
    describe "maximum'" $ do
        it "return the maximum number from list (or Nothing)" $ do
            maximum' [1, 2, 3] == Just 3

        it "return the maximum number from list (or Nothing)" $ do
            maximum' [1, 5, 3] == Just 5
        
        it "return the maximum number from list (or Nothing)" $ do
            maximum' [1] == Just 1
        
        it "return the maximum number from list (or Nothing)" $ do
            maximum' [] == Nothing
    
    describe "reverse'" $ do
        it "return reversed list" $ do
            reverse' [1, 2, 3] == [3, 2, 1]

        it "return reversed list" $ do
            reverse' [1] == [1]
        
        it "return reversed list" $ do
            reverse' [] == []
    
    describe "filter'" $ do
        it "return filtered (on function) list" $ do
            filter' (>3) [1, 5, 3, 2] == [5]
        
        it "return filtered (on function) list" $ do
            filter' (<5) [1, 5, 3, 2] == [1, 3, 2]
        
        it "return filtered (on function) list" $ do
            filter' (<0) [1, 5, 3, 2] == []
    
    describe "map'" $ do
        it  "function in every element from list" $ do
            map' (* 2) [1, 2, 3] == [2, 4, 6]
        
        it  "function in every element from list" $ do
            map' (* 2) [] == []
        
    describe "head'" $ do
        it "return the head of list (or Nothing)" $ do
            head' [1, 2, 3] == Just 1

        it "return the head of list (or Nothing)" $ do
            head' [] == Nothing

    describe "last'" $ do
        it "return the last element of list (or Nothing)"  $ do
            last' [1, 2, 3] == Just 3
         
        it "return the last element of list (or Nothing)"  $ do
            last' [] == Nothing
    
    describe "take'" $ do
        it "take first n elements from list" $ do
            take' 3 [1, 2, 3, 4] == [1, 2, 3]
        
        it "take first n elements from list" $ do
            take' 5 [1, 2] == [1, 2]
        
    describe "take''" $ do
        it "take first n elements from list" $ do
            take'' 3 [1, 2, 3, 4] == [1, 2, 3]
        
        it "take first n elements from list" $ do
            take'' 5 [1, 2] == [1, 2]
    
-- Number 3
    describe "quicksort" $ do
        it "sorted array" $ do
            quicksort [5, 3, 4, 2, 1] == [1, 2, 3, 4, 5]
        
        it "sorted array" $ do
            quicksort [] == []

    describe "insert" $ do
        it "add element by sorted array" $ do
            insert 3 [1, 2, 4, 5] == [1, 2, 3, 4, 5]

    describe "insertionSort" $ do
        it "sorted array" $ do
            insertionSort [1, 3, 2, 4, 3, 5] == [1, 2, 3, 3, 4, 5]

-- Number 4
    describe "myZipWith" $ do
        it "zip function" $ do
            myZipWith (+) [1, 3] [2, 4] == [3, 7]
    
    describe "fibs" $ do
        if "test inf lib function" $ do
            take 5 fibs == [0, 1, 1, 2, 3]
    
    describe "fibSum" $ do
        it "sum fibs[:ind]" $ do
            fibSum [1, 2, 3, 4, 5] == 12S

-- Number 5
    describe "bfs" $ do
        it "bfs" $ do
            bfs (Node 2 [Node 3 [Node 5 []], Node 1 []]) == [2, 3, 1, 5]

    describe "dfs" $ do
        it "dfs" $ do
            dfs (Node 2 [Node 3 [Node 5 []], Node 1 []]) == [2, 3, 5, 1]
