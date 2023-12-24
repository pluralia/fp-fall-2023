module TestSpec where

import Test.Hspec
import MyLib
import Data.Functor.Identity
import Data.Monoid (Sum(..))

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
    
    it "Task 9. Writer" $ do
        let tree1 = Node 5 
                        (Node 4 
                              (Node 3 
                                    (Node 2 
                                          (Node 1 
                                                Leaf Leaf
                                          ) 
                                           Leaf
                                    ) 
                                    Leaf
                              ) 
                              (Node 11 Leaf Leaf)
                        )
                        (Node 12 
                              (Node 13 Leaf Leaf) 
                              (Node 14 Leaf Leaf)
                        )
            tree2 = Leaf
            tree3 = Node 5
                         (Node 50 
                               (Node 10 Leaf Leaf) 
                               Leaf
                         )
                         (Node 150 Leaf Leaf)
      
        sumAndTraceInOrder tree1 `shouldBe` (Writer' (Identity [1, 2, 3, 4, 11, 5, 13, 12, 14], Sum 65) :: Writer' (Sum Int) [Int])
        sumAndTraceInOrder tree2 `shouldBe` (Writer' (Identity [], Sum 0) :: Writer' (Sum Int) [Int])
        sumAndTraceInOrder tree3 `shouldBe` (Writer' (Identity [10, 50, 5, 150], Sum 215) :: Writer' (Sum Int) [Int])
    
    it "Task 10. Reader'" $ do
        testEvalExpr `shouldBe` (Just 5 :: Maybe Int)
        testEvalStmts `shouldBe` (Just 9 :: Maybe Int)
        