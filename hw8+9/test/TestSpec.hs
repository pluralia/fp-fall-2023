module TestSpec (spec) where
import           Control.Monad.Writer.Lazy
import           Data.Functor.Identity
import           Data.Monoid               ()
import           MyHW8
import           Test.Hspec

spec :: Spec
spec = do

-- Task 7

    describe "pythagoreanTriple" $ do

        it "check correctness" $ do
            pythagoreanTriple 3 `shouldBe` ([] :: [(Integer, Integer, Integer)])
            pythagoreanTriple 5 `shouldBe` ([(3, 4, 5)] :: [(Integer, Integer, Integer)])
            pythagoreanTriple 10 `shouldBe` ([(3,4,5), (6,8,10)] :: [(Integer, Integer, Integer)])

-- Task 9

    describe "Monad Writer" $ do
        let m1  = Writer' (Identity 1, "Number 1")
        let m2  = Writer' (Identity 5, "Number 5")
        let m3 = Writer' (Identity "Late", "Simple string")
        let m4 = Writer' (Identity (+1),  "Add 1")
        let m5 = Writer' (Identity (*100), "Mult 100")
        let m6 = Writer' (Identity head, "Head")
        let m = Writer' (Identity (1, const ""), "Best monad") :: Writer' String (Int, String -> String)

        it "Functor `Writer'`" $ do
            fmap (*3) m1 `shouldBe` (Writer' (Identity 3,  "Number 1")  :: Writer' String Int)
            fmap (>1) m2 `shouldBe` (Writer' (Identity True,  "Number 5") :: Writer' String Bool)
            fmap (++ "Very ") m3 `shouldBe` (Writer' (Identity "LateVery ",  "Simple string") :: Writer' String String)

        it "Applicative `Writer'`" $ do
            m4 <*> m1 `shouldBe` (Writer' (Identity 2,  "Add 1Number 1") :: Writer' String Int)
            m6 <*> m3 `shouldBe` (Writer' (Identity 'L',  "HeadSimple string") :: Writer' String Char)
            m5 <*> m2 `shouldBe` (Writer' (Identity 500,  "Mult 100Number 5") :: Writer' String Int)

        it "Monad `Writer'`" $ do
            (m1 >>= return) `shouldBe` (Writer' (Identity 1, "Number 1") :: Writer' String Int)
            (m1 >>= const m2) `shouldBe` (Writer' (Identity 5, "Number 1Number 5") :: Writer' String Int)

        it "MonadWriter: tell" $ do
            tell "Log" `shouldBe` (Writer' (Identity (), "Log") :: Writer' String ())

        it "MonadWriter: listen" $ do
            listen m1 `shouldBe` Writer' (Identity (1, "Number 1"), "Number 1")

        it "MonadWriter: pass" $ do
            pass m `shouldBe` Writer' (Identity 1, "")

    describe "sumAndTraceInOrder" $ do
        let tree1 = Node 5 Leaf Leaf :: BinaryTree Int
        let lTree = Node 2 Leaf (Node 3 Leaf Leaf)
        let rTree = Node 4 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)
        let tree2 = Node 1 lTree rTree

-- tree2
--       1
--    /    \
--   2      4
--  / \    /  \
-- L   3  5    6

        it "check trees" $ do
            sumAndTraceInOrder tree1 `shouldBe` (Writer' (Identity [5], Sum 5) :: Writer' (Sum Int) [Int])
            sumAndTraceInOrder tree2 `shouldBe` (Writer' (Identity [1, 2, 3, 4, 5, 6], Sum 21) :: Writer' (Sum Int) [Int])

-- Task 10

    describe "Monad Reader'" $ do
        let m1  = Reader' (\_ -> Identity 1)
        let m2  = Reader' (\r -> Identity $ "abc" ++ r) :: Reader' String String
        let m3 = Reader' (\_ -> Identity id)
        let m4 = Reader' (\_ -> Identity succ)
        let m5 = Reader' (\_ -> Identity 6)
        let m6 x= Reader' (\_ -> Identity x + 100)


        it "Functor `Reader'`" $ do
            runReader' (fmap (+1) m1) (10 :: Int) `shouldBe` Identity (2 :: Int)
            runReader' (fmap (const "New") m2) "cde" `shouldBe` Identity "New"
            runReader' (fmap id m2) "def" `shouldBe` Identity "abcdef"

        it "Applicative `Reader'`" $ do
            runReader' (m3 <*> m1) (10 :: Int) `shouldBe` Identity (1 :: Int)
            runReader' (m4 <*> m1) (10 :: Int) `shouldBe` Identity (2 :: Int)
            runReader' (m3 <*> m2) "def" `shouldBe` (Identity "abcdef" :: Identity String)

        it "Monad `Reader'`" $ do
            runReader' (m1 >>= return) "" `shouldBe` Identity 1
            runReader' (m1 >>= const m5) (10 :: Int) `shouldBe` (Identity 6 :: (Identity Int))
            runReader' (m1 >>= m6) (10 :: Int) `shouldBe` Identity (101 :: Int)

-- Task 9
    describe "Monad Reader'" $ do

        it "testEvalExpr" $ do
            testEvalExpr `shouldBe` Just 5

        it "testEvalStmts" $ do
            testEvalStmts `shouldBe` Just 9
