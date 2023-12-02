{-# LANGUAGE TupleSections #-}

module TestSpec where

import Test.Hspec
import MyLib

import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import qualified Data.Map.Strict as M

k1 :: Int -> StateWithError s String String
k1 x = MyState (Right $ "Value is " ++ show x,)

k2 :: String -> StateWithError s String Int
k2 x = MyState (Right $ length x,)


f :: Template -> Template -> Reader Environment String
f (Include _ defs) temp2 = do 
  mutResolve defs $ do 
    resolve temp2
f _                _     = pure ""

spec :: Spec
spec = do

    let myTemp1 = Var (Text "x")
    let myTemp2 = Quote (Text "try")
    let myTemp3 = Include (Text "fix") [Definition (Text "a") (Text "3"), 
                                        Definition (Text "b") (Text "2"),
                                        Definition (Text "c") (Text "1")]
    let myTemp4 = Compound [Text "MyTemp: z = ", Var (Text "z"), Text ", myTemp2 = ", Quote (Text "error")]
    let myTemp5 = Compound [Text "a = ", Var (Text "a"), Text ", b = ", Var (Text "b"), Text ", c = ", Var (Text "c")]
    let myTemp6 = Var (Text "a")

    let myEnv1 = Env {
        templs = M.fromList [("try", myTemp1), ("fix", myTemp2), ("error", myTemp6)],
        vars = M.fromList [("x", "42"), ("y", "0"), ("z", "8")]
    }

    describe "Template" $ do
        it "small test" $ do
            runReader (resolve myTemp1) myEnv1 `shouldBe` ("42"                                         :: String)
            runReader (resolve myTemp2) myEnv1 `shouldBe` ("Var (Text \"x\")"                           :: String)
        it "Include and Compound test" $ do
            runReader (resolve myTemp3) myEnv1 `shouldBe` ("Quote (Text \"try\")"                       :: String)
            runReader (resolve myTemp4) myEnv1 `shouldBe` ("MyTemp: z = 8, myTemp2 = Var (Text \"a\")"  :: String)
        it "change Environment" $ do
            runReader (f myTemp3 myTemp5) myEnv1 `shouldBe` ("a = 3, b = 2, c = 1"                      :: String)
            runReader (f myTemp3 myTemp6) myEnv1 `shouldBe` ("3"                                        :: String)

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
