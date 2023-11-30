{-# LANGUAGE TupleSections #-}

module TestSpec where

import Test.Hspec
import MyLib

import           Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

k1 :: Int -> StateWithError s String String
k1 x = MyState (Right $ "Value is " ++ show x,)

k2 :: String -> StateWithError s String Int
k2 x = MyState (Right $ length x,)

spec :: Spec
spec = do
    describe "Fibonacci test" $ do
        it "start values" $ do
            evalState (fib 0) M.empty               `shouldBe` (1 :: Integer)
            evalState (fib 1) M.empty               `shouldBe` (1 :: Integer)
        it "other values" $ do
            evalState (fib 5)   M.empty `shouldBe` (8 :: Integer)
            evalState (fib 100) M.empty `shouldBe` (573147844013817084101 :: Integer)

    describe "My StateWithError monad" $ do

        let env1 = M.fromList [(1, "It's"), (2, "My"), (3, "Life")] :: M.Map Int String
        let env2 = M.fromList [("first", 52), ("second", 127)]      :: M.Map String Int
        let s1 = MyState (\v-> (Right $ env1 M.! v, v))
        let s2 = MyState (\v-> (Right $ env2 M.! v + 5, v))
        let s3 = MyState (Left "foo",)
        let f1 = MyState (Right $ \t -> t ++ "!",)

        it "Functor instance" $ do
            fst (runMyState (fmap show s2) "second") `shouldBe` (Right "132" :: Either String String)
            fst (runMyState (fmap (>5) s2) "first")  `shouldBe` (Right  True :: Either String Bool)
        it "Applicative instance" $ do
            fst (runMyState (f1 <*> s1) 3)  `shouldBe` (Right "Life!"   :: Either String String)
            fst (runMyState (f1 <*> s3) "") `shouldBe` (Left "foo"      :: Either String String)
        it "Monad instance" $ do
            fst (runMyState (s1 >>= k2) 1)          `shouldBe` (Right 4              :: Either String Int)
            fst (runMyState (s2 >>= k1) "second")   `shouldBe` (Right "Value is 132" :: Either String String)
            fst (runMyState (s3 >>= k2) "")         `shouldBe` (Left "foo"           :: Either String Int)
