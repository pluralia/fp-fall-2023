module TestSpec (spec) where

import           MyLib
import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader()
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import qualified System.Random as R -- cabal install --lib  random
import           Data.Functor.Identity (Identity(..))
import           Data.Maybe ()

import Test.Hspec
  (
    Spec
  , it
  , describe
  , shouldBe
  , shouldSatisfy
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "match && filterOne tests" $ do
        let 
          rules :: [Rule]
          rules =
            [ Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            , Rule Reject (IPAddress "10.0.0.1") (IPAddress "192.168.1.1")
            ]

          packet1, packet2 :: Packet
          packet1 = Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
          packet2 = Packet (IPAddress "10.0.0.1") (IPAddress "192.168.1.1")

        filterOne rules packet1 `shouldBe` WriterT (Identity (Just Packet {pSource = IPAddress "192.168.1.1",pDestination = IPAddress "192.168.2.2"}, [Log {count = 1,msg = "Packet {pSource = IPAddress \"192.168.1.1\", pDestination = IPAddress \"192.168.2.2\"} accepted"}]))
        filterOne rules packet2 `shouldBe` WriterT (Identity (Nothing, [Log {count = 1, msg = "Packet {pSource = IPAddress \"10.0.0.1\", pDestination = IPAddress \"192.168.1.1\"} rejected"}]))
      
      it "mergeEntries tests" $ do
        let
          log1 = [Log 1 "Some error"]
          log2 = [Log 2 "Some error"]
          log3 = [Log 3 "All good"]

        snd (runWriter (mergeEntries log1 log2)) `shouldBe` [Log 3 "Some error"]
        snd (runWriter (mergeEntries log1 log3)) `shouldBe` [Log 1 "Some error", Log 3 "All good"]

      it "mergeEntries && groupSame tests" $ do
        let
          val = [1, 2, 3, 4] :: [Int]
          (_, logs) = runWriter $ groupSame [] mergeEntries val (\x -> tell [Log 2 (show x)])
          (r, _) = runWriter resultWriter
        r    `shouldBe` [2,4,6,8,10]
        logs `shouldBe` [Log 2 "1", Log 2 "2", Log 2 "3", Log 2 "4"]

      it "mergeEntries && groupSame tests from Git" $ do
        let
          rules = [Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.1.2")]
          packets = [Packet (IPAddress "192.168.1.1") (IPAddress "192.168.1.2"), Packet (IPAddress "192.168.1.3") (IPAddress "192.168.1.4"), Packet (IPAddress "192.168.1.3") (IPAddress "192.168.1.4")]
          result = snd $ runWriter $ groupSame [] mergeEntries packets (filterOne rules)
        show result `shouldBe`
          "[Log {count = 1, msg = \"Packet {pSource = IPAddress \\\"192.168.1.1\\\", pDestination = IPAddress \\\"192.168.1.2\\\"} accepted\"},Log {count = 2, msg = \"Packet {pSource = IPAddress \\\"192.168.1.3\\\", pDestination = IPAddress \\\"192.168.1.4\\\"} rejected\"}]"

      it "filterAll returns filtered packets along with logs" $ do
        let
          rules = [Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")]
          packet1 = [Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2"), Packet (IPAddress "192.168.3.3") (IPAddress "192.168.4.4")]
          expected = [Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")]

        fst (runWriter (filterAll rules packet1)) `shouldBe` expected
        fst (runWriter (filterAll rules []))      `shouldBe` []

      it "lookupVar tests" $ do
        let
          environment1 = Env { templs = M.empty, vars = M.fromList [("AB", "CD")] }
          environment2 = Env { templs = M.empty, vars = M.empty }
        
        lookupVar "AB" environment1    `shouldBe` Just "CD"
        lookupVar "empty" environment2 `shouldBe` Nothing
        
      it "lookupTemplate tests" $ do
        let
          template1 = Text "RNA secondary structure important"
          environment1 = Env { templs = M.singleton "RNA" template1, vars = M.empty }
          
        lookupTemplate "RNA" environment1  `shouldBe` Just template1
        lookupTemplate "mRNA" environment1 `shouldBe` Nothing

      it "addDefs tests" $ do
        let 
          testVars = M.fromList [("tRNA", "rRNA")]

          environment1 = Env { templs = M.empty, vars = M.empty }
          environment2 = Env { templs = M.empty, vars = testVars }

          newVars = M.fromList [("ab", "12"), ("cd", "34")]

          addEnvironment1 = addDefs newVars environment1
          addEnvironment2 = addDefs newVars environment2

          expected = M.union testVars newVars

        vars addEnvironment1 `shouldBe` newVars
        vars addEnvironment2 `shouldBe` expected

      it "whileM_ tests" $ do
        runState example 5 `shouldBe` ((),0)

      it "getOne tests" $ do
        let 
          bounds = (1, 300) :: (Int, Int)
          (value, _) = runState (getOne bounds) (R.mkStdGen 36 :: R.StdGen)
        value `shouldSatisfy` (\x -> x >= fst bounds && x <= snd bounds)
      
      it "makeRandomValueST tests" $ do
        makeRandomValueST (R.mkStdGen 23)  `shouldBe` makeRandomValue (R.mkStdGen 23)
        makeRandomValueST (R.mkStdGen 50)  `shouldBe` makeRandomValue (R.mkStdGen 50)
        makeRandomValueST (R.mkStdGen 100) `shouldBe` makeRandomValue (R.mkStdGen 100)

      it "setVar tests" $ do
        let
          initState = M.fromList [("A", 1), ("B", 2)]
          upState1 = execState (setVar "A" 3) initState
          upState2 = execState (setVar "D" 4) initState
        M.lookup "A" upState1 `shouldBe` Just 3
        M.lookup "D" upState2 `shouldBe` Just 4

      it "incVar tests" $ do
        let
          initState = M.fromList [("A", 1), ("B", 2)]
          upState1 = execState (incVar "A" 3) initState
        M.lookup "A" upState1 `shouldBe` Just 4

      it "getVar tests" $ do
        let
          initState = M.fromList [("A", 1), ("B", 2)]
        evalState (getVar "A") initState `shouldBe` 1

      it "fib tests" $ do
        evalState (fib 0) M.empty  `shouldBe` 1
        evalState (fib 5) M.empty  `shouldBe` 8
        evalState (fib 10) M.empty `shouldBe` 89
        evalState (fib 15) M.empty `shouldBe` 987

      it "State и Either tests" $ do
        runStateWithError (divideWithCatch 10 2) 10 `shouldBe` Right 5
        runStateWithError (divideWithCatch 0 2) 0   `shouldBe` Right 0
        runStateWithError (divideWithCatch 10 0) 10 `shouldBe` Left "Caught an error: Division by zero!"
