module TestSpec where

import Test.Hspec
import MyLib
import Data.Functor.Identity
import qualified Data.Map as M
import Control.Exception (evaluate)

spec :: Spec
spec = do
    -- Task 1 a
    describe "Maybe'" $ do
        it "traverse with Just'" $ do
            traverse Just (Just' 5) `shouldBe` (Just' <$> Just (5 :: Int))

        it "foldr with Just'" $ do
            foldr (+) 0 (Just' 5) `shouldBe` (5 :: Int)

        it "fmap with Just'" $ do
            fmap (+1) (Just' 5) `shouldBe` Just' (6 :: Int)

        it "foldr with Nothing'" $ do
            foldr (+) 0 Nothing' `shouldBe` (0 :: Int)

            
    describe "List" $ do
        it "traverse with non-empty List" $ do
            traverse Just (List [1, 2, 3]) `shouldBe` (List <$> traverse Just ([1, 2, 3] :: [Int]))

        it "foldr with non-empty List" $ do
            foldr (+) 0 (List [1, 2, 3]) `shouldBe` (6 :: Int)

        it "fmap with non-empty List" $ do
            fmap (+1) (List [1, 2, 3]) `shouldBe` List ([2, 3, 4] :: [Int])

        it "foldr with empty List" $ do
            foldr (+) 0 (List []) `shouldBe` (0 :: Int)   

-- Task 2
    describe "rejectWithNegatives" $ do
        it "returns Just for a list with no negative numbers" $ do
            rejectWithNegatives [1, 2, 3, 4, 5] `shouldBe` Just ([1, 2, 3, 4, 5] :: [Int])

        it "returns Nothing for a list with at least one negative number" $ do
            rejectWithNegatives ([1, -2, 3, 4, 5] :: [Int]) `shouldBe` Nothing 

        it "returns Just for an empty list" $ do
            rejectWithNegatives ([] :: [Int]) `shouldBe` Just ([] :: [Int])

-- Task 3 
    -- describe "transpose" $ do
    --     it "transpose an emty matrix" $ do
    --         transpose ([] :: [[Int]]) `shouldBe` []

    --     it "transpose 1x1 matrix" $ do
    --         transpose [[1] :: [Int]] `shouldBe` [[1]]

    --     it "transpose 2x2 matrix" $ do
    --         transpose [[1, 2] :: [Int], [3, 4] :: [Int]] `shouldBe` [[1, 3], [2, 4]]

-- Task 5
    describe "WithData" $ do
        
        it "checks the Monad instance" $ do
            let wd = WithData (*2) :: WithData Int Int
            runWithData (wd >>= \x -> return (x + 1)) 3 `shouldBe` 7

        it "checks the MonadFail instance" $ do
            let wd = fail "This is an error" :: WithData Int Int
            evaluate (runWithData wd 3) `shouldThrow` errorCall "Sonething went wrong"

    -- Task 7 
    describe "pythagoreanTriples" $ do
        it "returns all Pythagorean triples for n=5" $ do
            pythagoreanTriples 5 `shouldBe` [(3, 4, 5), (4, 3, 5)]

        it "returns an empty list for n=2" $ do
            pythagoreanTriples 2 `shouldBe` []
    -- Task 8
    -- describe "ReturnableCalculation Monad" $ do
    --     it "realReturn should return the given value" $ do
    --         let rc = realReturn 42 :: ReturnableCalculation Int
    --         rc `shouldBe` ReturnableCalculation (Just 42, False)

    --     it "realReturn should ignore subsequent computations" $ do
    --         let rc = do
    --                 _ <- realReturn (42 :: Int)
    --                 pure 100 :: ReturnableCalculation Int
    --         rc `shouldBe` ReturnableCalculation (Just 42, False)

    --     it "should perform computations until realReturn is called" $ do
    --         let rc = do
    --                 let x = 40
    --                     y = 2
    --                 _ <- realReturn ((x + y) :: Int)
    --                 pure 100 :: ReturnableCalculation Int
    --         rc `shouldBe` ReturnableCalculation (Just 42, False)

    --     it "returnExample should return 42" $ do
    --         let returnExample = do   
    --                 let a1 = 40
    --                     b = 2
    --                 _ <- realReturn ((a1 + b) :: Int)
    --                 let  a2 = 0 :: Int
    --                 if (a2 == 0) :: Bool
    --                 then pure 200
    --                 else realReturn 0
    --         returnExample `shouldBe` ReturnableCalculation (Just (42 :: Int), False)
-- Task 9 a
    describe "Writer' tests" $ do
        it "checks fmap for Writer'" $ do
            let writer' = Writer' (Identity 3, "Hello")
            let f x = x + 2
            let result = fmap f writer'
            runWriter' result `shouldBe` (Identity (5 :: Int), "Hello")

        it "checks Applicative pure for Writer'" $ do
            let result = pure 3 :: Writer' String Int
            runWriter' result `shouldBe` (Identity 3, "")

        it "checks Monad bind for Writer'" $ do
            let writer' = Writer' (Identity 3, "Hello")
            let f x = Writer' (Identity (x + 2), " World")
            let result = writer' >>= f
            runWriter' result `shouldBe` (Identity (5 :: Int), "Hello World")
-- Task 9 b
-- tell, listen, pass - Variable not in scope:
    -- describe "MonadWriter for Writer' tests" $ do
    --     it "checks tell for Writer'" $ do
    --         let writer' = tell "Hello, " >> tell "world!" :: Writer' String ()
    --         let (_, w) = runWriter' writer'
    --         w `shouldBe` "Hello, world!"

    --     it "checks listen for Writer'" $ do
    --         let writer' = listen (tell "Hello, world!" >> return 42) :: Writer' String (Int, String)
    --         let (Identity (a, w1), w2) = runWriter' writer'

    --         a `shouldBe` 42
    --         w1 `shouldBe` "Hello, world!"
    --         w2 `shouldBe` "Hello, world!"

    --     it "checks pass for Writer'" $ do
    --         let writer' = pass (tell "Hello, " >> return ((), (++ "world!"))) :: Writer' String ()
    --         let (_, w) = runWriter' writer'
    --         w `shouldBe` "Hello, world!"

-- Task 9 c
-- getSum Variable not in scope
    -- describe "sumAndTraceInOrder" $ do

    --     it "returns an empty list and sum for an empty tree" $ do
    --         let tree = Leaf :: BinaryTree Int
    --         let result = runWriter' (sumAndTraceInOrder tree)
    --         let (Identity lst, s) = result
    --         lst `shouldBe` []
    --         getSum s `shouldBe` 0

    --     it "returns a list with one element and its sum for a tree with one node" $ do
    --         let tree = Node 5 Leaf Leaf :: BinaryTree Int
    --         let result = runWriter' (sumAndTraceInOrder tree)
    --         let (Identity lst, s) = result
    --         lst `shouldBe` [5]
    --         getSum s `shouldBe` 5

    --     it "returns a sorted list and its sum for a complete binary tree" $ do
    --         let tree = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf) :: BinaryTree Int
    --         let result = runWriter' (sumAndTraceInOrder tree)
    --         let (Identity lst, s) = result
    --         lst `shouldBe` [1,2,3]
    --         getSum s `shouldBe` 6

-- Task 10 a
    describe "Reader'" $ do
        it "fmap applies a function to the result" $ do
            let reader = Reader' $ \r -> Identity r
            let reader' = fmap (*2) reader
            runIdentity (runReader' reader' 5) `shouldBe` (10 :: Int)

        it "pure returns a Reader' that ignores its environment" $ do
            let reader = pure 5 :: Reader' Int Int
            runIdentity (runReader' reader 10) `shouldBe` 5

        it ">>= chains computations that depend on the environment" $ do
            let reader = Reader' $ \r -> Identity r
            let reader' = reader >>= \x -> return (x * 2)
            runIdentity (runReader' reader' 5) `shouldBe` (10 :: Int)

-- Task 10 b
-- local Variable not in scope
    -- describe "Reader'" $ do
    --     it "ask returns the environment" $ do
    --         let reader = Reader' $ \r -> Identity r
    --         runIdentity (runReader' reader 5) `shouldBe` 5

    --     it "local modifies the environment" $ do
    --         let reader = Reader' $ \r -> Identity r 
    --         let reader' = local (+1) reader  
    --         runIdentity (runReader' reader' 5) `shouldBe` 6

    --     it "local does not affect subsequent computations" $ do
    --         let reader = Reader' $ \r -> Identity r 
    --         let reader' = local (+1) reader >> Reader' (\r -> Identity r) 
    --         runIdentity (runReader' reader' 5) `shouldBe` 5

-- Task 10 c
    describe "eval" $ do
            it "evaluates a primary expression with a variable" $ do
                let env = M.fromList [("x", 3)]
                runIdentity (runReader' (eval (Primary (Var "x"))) env) `shouldBe` Just 3

            it "evaluates a primary expression with a value" $ do
                let env = M.empty
                runIdentity (runReader' (eval (Primary (Val 2))) env) `shouldBe` Just 2

            it "evaluates a binary expression" $ do
                let env = M.fromList [("x", 3)]
                let expr' = Binary (Primary (Val 2)) (Primary (Var "x"))
                runIdentity (runReader' (eval expr') env) `shouldBe` Just 5

            it "returns Nothing if a variable is not found" $ do
                let env = M.empty
                runIdentity (runReader' (eval (Primary (Var "x"))) env) `shouldBe` Nothing

            it "returns Nothing if a binary expression contains a variable that is not found" $ do
                let env = M.fromList [("x", 3)]
                let expr' = Binary (Primary (Val 2)) (Primary (Var "y"))
                runIdentity (runReader' (eval expr') env) `shouldBe` Nothing

    describe "evalStmts" $ do
            it "evaluates a list of statements and returns the result of the last one" $ do
                let stmts = [ Stmt "x" (Primary (Val 2))
                            , Stmt "y" (Binary (Primary (Var "x")) (Primary (Val 3)))
                            ]
                runIdentity (runReader' (evalStmts stmts) M.empty) `shouldBe` Just 5

            it "returns Nothing if a statement contains a variable that is not found" $ do
                let stmts = [ Stmt "x" (Primary (Val 2))
                            , Stmt "y" (Primary (Var "z"))
                            ]
                runIdentity (runReader' (evalStmts stmts) M.empty) `shouldBe` Nothing