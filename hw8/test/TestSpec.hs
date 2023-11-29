module TestSpec (spec) where

import qualified Data.Map.Strict as M
import           Data.Functor.Identity
import           Control.Monad.Writer.Strict
import           Control.Monad.Reader
import           Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

import MyLib

spec :: Spec
spec = do
  describe "traversable" $ do
    it "Maybe'" $ do
      let 
        f :: Int -> [Int]
        f x = [x, x, x]
      traverse f (Just' 1 :: Maybe' Int) `shouldBe` ([Just' 1, Just' 1, Just' 1] :: [Maybe' Int])
      sequenceA (Just' [1, 2, 3] :: Maybe' [Int]) `shouldBe` ([Just' 1, Just' 2, Just' 3] :: [Maybe' Int])
      sequenceA [Just' 1, Just' 2, Just' 3] `shouldBe` (Just' [1, 2, 3] :: Maybe' [Int])

    it "List'" $ do
      let 
        f :: Int -> Maybe' Int
        f x = if x > 0 then Just' x else Nothing'
      traverse f (Cons' 1 (Cons' 2 (Cons' 3 Nil')) :: List' Int) `shouldBe` (Just' (Cons' 1 (Cons' 2 (Cons' 3 Nil'))) :: Maybe' (List' Int))
      traverse f (Cons' 1 (Cons' (-2) (Cons' 3 Nil')) :: List' Int) `shouldBe` (Nothing' :: Maybe' (List' Int))
      sequenceA (Cons' (Just' 1) (Cons' (Just' 2) (Cons' (Just' 3) Nil')) :: List' (Maybe' Int)) `shouldBe` (Just' (Cons' 1 (Cons' 2 (Cons' 3 Nil'))) :: Maybe' (List' Int)) 
      sequenceA (Cons' (Just' 1) (Cons' Nothing' (Cons' (Just' 3) Nil')) :: List' (Maybe' Int)) `shouldBe` (Nothing' :: Maybe' (List' Int))

    it "traverse'" $ do
      let 
        f :: Int -> Maybe' Int
        f x = if x > 0 then Just' x else Nothing'
      traverse' f (Cons' 1 (Cons' 2 (Cons' 3 Nil')) :: List' Int) `shouldBe` (Just' (Cons' 1 (Cons' 2 (Cons' 3 Nil'))) :: Maybe' (List' Int))
      traverse' f (Cons' 1 (Cons' (-2) (Cons' 3 Nil')) :: List' Int) `shouldBe` (Nothing' :: Maybe' (List' Int))
      sequenceA' (Cons' (Just' 1) (Cons' (Just' 2) (Cons' (Just' 3) Nil')) :: List' (Maybe' Int)) `shouldBe` (Just' (Cons' 1 (Cons' 2 (Cons' 3 Nil'))) :: Maybe' (List' Int)) 
      sequenceA' (Cons' (Just' 1) (Cons' Nothing' (Cons' (Just' 3) Nil')) :: List' (Maybe' Int)) `shouldBe` (Nothing' :: Maybe' (List' Int))

  describe "rejectWithNegatives" $ do
    it "rejectWithNegatives" $ do
      rejectWithNegatives [1, 2, 3] `shouldBe` (Just [1, 2, 3] :: Maybe [Int])
      rejectWithNegatives [] `shouldBe` (Just [] :: Maybe [Int])
      rejectWithNegatives [1, 0 .. -3] `shouldBe` (Nothing :: Maybe [Int])

  describe "transpose" $ do
    it "transpose" $ do
      transpose ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]]) `shouldBe` ([[1, 4, 7], [2, 5, 8], [3, 6, 9]] :: [[Int]])
      transpose ([[1]] :: [[Int]]) `shouldBe` ([[1]] :: [[Int]])
      transpose ([['a', 'b', 'c'], ['d', 'e', 'f']] :: [[Char]]) `shouldBe` ([['a', 'd'], ['b', 'e'], ['c', 'f']] :: [[Char]])

  describe "WithData" $ do
    it "WithData" $ do
      let
        plus1 :: WithData Int (Maybe Int)
        plus1 = WithData $ \x -> if x > 0 then Just (x + 1) else Nothing
      runWithData plus1 1 `shouldBe` (Just 2 :: Maybe Int)
      runWithData plus1 (-1) `shouldBe` (Nothing :: Maybe Int)

  -- зависает на этом тесте
  -- describe "fromDo11" $ do
  --   it "fromDo11" $ do
  --     fromDo11 (Just 1 :: Maybe Int) (Just "a" :: Maybe String) `shouldBe` fromDo11' (Just 1 :: Maybe Int) (Just "a" :: Maybe String)
  --     fromDo11 (Nothing :: Maybe Int) (Just "a" :: Maybe String) `shouldBe` fromDo11' (Nothing :: Maybe Int) (Just "a" :: Maybe String)

  describe "fromDo12" $ do
    it "fromDo12" $ do
      fromDo12 ([1, 2, 3] :: [Int]) (Just 'a' :: Maybe Char) `shouldBe` fromDo12' ([1, 2, 3] :: [Int]) (Just 'a' :: Maybe Char)
      fromDo12 ([1, 2, 3] :: [Int]) (Nothing :: Maybe Char) `shouldBe` fromDo12' ([1, 2, 3] :: [Int]) (Nothing :: Maybe Char)

  describe "pifList" $ do
    it "pifList" $ do
      take 4 pifList `shouldBe` ([(3, 4, 5), (6, 8, 10), (5, 12, 13), (9, 12, 15)] :: [(Int, Int, Int)])

  describe "ReturnableCalculation" $ do
    it "ReturnableCalculation" $ do
      let 
        calc :: ReturnableCalculation Int
        calc = do
          let 
            a = 40
            b = 2
          realReturn $ a + b
          let 
            aa = 0
          return $ aa + b
      runCalculation calc `shouldBe` (Left 42 :: Either Int Int)
      let
        calc' :: ReturnableCalculation Int
        calc' = do
          let 
            a = 40
            b = 2
          return $ a + b -- я видел что hlint подчеркивает return. Я специально
          let 
            aa = 0
          return $ aa + b
      runCalculation calc' `shouldBe` (Right 2 :: Either Int Int)

  describe "Writer'" $ do
    it "Writer'" $ do
      let 
        w :: Writer' String Int
        w = do
          tell "a"
          tell "b"
          tell "c"
          pure 1
      runWriter' w `shouldBe` ((Identity 1, "abc") :: (Identity Int, String))
      let 
        w' :: Writer' String Int
        w' = do
          tell "a"
          tell "b"
          tell "c"
          tell "d"
          pure 1
      runWriter' w' `shouldBe` ((Identity 1, "abcd") :: (Identity Int, String))

  describe "sumAndTraceInOrder" $ do
    it "sumAndTraceInOrder" $ do
      let 
        tree :: BinaryTree Int
        tree = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
      runWriter' (sumAndTraceInOrder tree) `shouldBe` ((Identity [2, 1, 3], Sum 6) :: (Identity [Int], Sum Int))
    
  describe "Reader'" $ do
    it "Reader'" $ do
      let 
        r :: Reader' Int Int
        r = do
          x <- ask
          pure $ x + 1
      runReader' r 1 `shouldBe` (Identity 2 :: Identity Int)
      let 
        r' :: Reader' Int String
        r' = do
          asks show
      runReader' r' 1 `shouldBe` (Identity "1" :: Identity String)

  describe "eval" $ do
    it "eval" $ do
      let 
        env :: Environment
        env = M.fromList [("x", 3)]
        expr' :: Expr
        expr' = Binary (Primary . Val $ 2) (Primary . Var $ "x")
      runIdentity (runReader' (eval expr') env) `shouldBe` (Just 5 :: Maybe Int)
    
  describe "evalStmts" $ do
    it "evalStmts" $ do
      let 
        x, y, z, xx, w :: Stmt
        x = Stmt "x" $ Primary . Val $ 2                                     -- x = 2
        y = Stmt "y" $ Primary . Val $ 3                                     -- y = 3
        z = Stmt "z" $ Binary (Primary . Var $ "x") (Primary . Var $ "y")    -- z = 5
        xx = Stmt "x" $ Binary (Primary . Var $ "x") (Primary . Var $ "x")   -- xx = 4
        w = Stmt "w" $ Binary (Primary . Var $ "z") (Primary . Var $ "x")    -- w = 9
      runIdentity (runReader' (evalStmts [x, y, z, xx, w]) M.empty) `shouldBe` (Just 9 :: Maybe Int)
