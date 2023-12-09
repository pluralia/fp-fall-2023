module TestSpec where

import Test.Hspec
import MyLib
import Data.Functor.Identity

spec :: Spec
spec = do
    it "Task 9. Writer" $ do
        let f1 = Writer' (Identity (*10),  "Func1 ")
            f2 = Writer' (Identity (+5), "Func2 ")
            x = Writer' (Identity 1, "x ")
            y = Writer' (Identity 5, "y ")

        fmap (*15) x `shouldBe` (Writer' (Identity 15, "x ") :: Writer' String Int)
        fmap (+50) y `shouldBe` (Writer' (Identity 55, "y ") :: Writer' String Int)
        fmap (<15) y `shouldBe` (Writer' (Identity True, "y ") :: Writer' String Bool)
        fmap (==0) x `shouldBe` (Writer' (Identity False, "x ") :: Writer' String Bool)
        
        f1 <*> x `shouldBe` (Writer' (Identity 10, "Func1 x "))
        f2 <*> y `shouldBe` (Writer' (Identity 10, "Func2 y "))
    
    it "Task 10. Reader'" $ do
        testEvalExpr `shouldBe` (Just 5 :: Maybe Int)
        testEvalStmts `shouldBe` (Just 9 :: Maybe Int)
        