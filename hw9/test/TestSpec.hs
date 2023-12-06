module TestSpec (spec) where

import Test.Hspec
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified System.Random as R
import MyLib

spec :: Spec
spec = do
  describe "logMsg" $ do
    it "should add a message to the log" $ do
      let 
        (_, logs) = runWriter $ logMsg "Test message"
      logs `shouldBe` [Log 1 "Test message"]

  describe "match" $ do
    it "returns correct result" $ do
      let 
        rules = [Rule Accept (IP 192 168 0 1) (IP 192 168 0 2), 
                 Rule Reject (IP 192 168 0 3) (IP 192 168 0 4)]
        packet1 = Packet (IP 192 168 0 1) (IP 192 168 0 2)
        packet2 = Packet (IP 192 168 0 3) (IP 192 168 0 4)
        packet3 = Packet (IP 192 168 0 5) (IP 192 168 0 6)
      match rules packet1 `shouldBe` Just (Rule Accept (IP 192 168 0 1) (IP 192 168 0 2))
      match rules packet2 `shouldBe` Just (Rule Reject (IP 192 168 0 3) (IP 192 168 0 4))
      match rules packet3 `shouldBe` Nothing

  describe "filterOne" $ do
    it "should filter a packet" $ do
      let 
        rules = [Rule Accept (IP 192 168 0 1) (IP 192 168 0 2), 
                 Rule Reject (IP 192 168 0 3) (IP 192 168 0 4)]
        packet1 = Packet (IP 192 168 0 1) (IP 192 168 0 2)
        packet2 = Packet (IP 192 168 0 3) (IP 192 168 0 4)
        packet3 = Packet (IP 192 168 0 5) (IP 192 168 0 6)
        (_, logs1) = runWriter $ filterOne rules packet1
        (_, logs2) = runWriter $ filterOne rules packet2
        (_, logs3) = runWriter $ filterOne rules packet3
      logs1 `shouldBe` [Log 1 "Packet from 192.168.0.1 to 192.168.0.2 is accepted"]
      logs2 `shouldBe` [Log 1 "Packet from 192.168.0.3 to 192.168.0.4 is rejected"]
      logs3 `shouldBe` [Log 1 "Packet from 192.168.0.5 to 192.168.0.6 is rejected"]

  describe "mergeEntries" $ do
    it "should merge duplicate entries in the log" $ do
      let 
        entries = [Log 1 "Message 1", Log 2 "Message 2", Log 3 "Message 3"]
        (_, merged) = runWriter $ mergeEntries [Log 1 "Message 1"] entries
      merged `shouldBe` [Log 2 "Message 1", Log 2 "Message 2", Log 3 "Message 3"]

  describe "groupSame" $ do
    it "should group values with the same log" $ do
      let 
        values = [1, 2, 3, 4, 5] :: [Int]
        (_, logs) = runWriter $ groupSame [] mergeEntries values (\x -> tell [Log 1 (show x)])
      logs `shouldBe` [Log 1 "1", Log 1 "2", Log 1 "3", Log 1 "4", Log 1 "5"]

  describe "resolve" $ do
    it "should resolve Text template" $ do
      let env = Env { templs = M.empty, vars = M.empty }
      runReader (resolve (Text "Hello, World!")) env `shouldBe` "Hello, World!"

    it "should resolve Var" $ do
      let env = Env { templs = M.empty, vars = M.fromList [("name", "Alice")] }
      runReader (resolve (Var (Text "name"))) env `shouldBe` "Alice"
      runReader (resolve (Var (Text "age"))) env `shouldBe` ""

  describe "getAny'" $ do
    it "returns a random value" $ do
      let 
        (value, _) = runState getAny' (R.mkStdGen 42)
      value `shouldBe` (1275548033995301424 :: Int)
      
  describe "getOne'" $ do
    it "returns a random value within the given bounds" $ do
      let 
        (value, _) = runState (getOne' (1, 10)) (R.mkStdGen 42)
      value `shouldBe` (1 :: Int)

  describe "makeRandomValueST" $ do
    it "returns a random MyType value" $ do
      let 
        (value, _) = makeRandomValueST (R.mkStdGen 42)
      value `shouldBe` MT 49 True 'p' (-34)

  describe "whileM_" $ do
    it "returns correct result" $ do
      let result = execState (whileM_' (gets (< 5)) (modify (+1))) (0 :: Int)
      result `shouldBe` (5 :: Int)

  describe "forM_'" $ do
    it "executes the body for each iteration while the condition is true" $ do
      let result = execState (forM_' (modify (+0), gets (< 5), modify (+1)) (return ())) (0 :: Int)
      result `shouldBe` (5 :: Int)

    it "does not execute the body if the condition is false" $ do
      let result = execState (forM_' (modify (+0), gets (< 0), modify (+1)) (return ())) (0 :: Int)
      result `shouldBe` (0 :: Int)

  describe "setVar" $ do
    it "sets the value of an existing variable" $ do
      let result = execState (setVar "x" 10) (M.fromList [("x", 0)])
      M.lookup "x" result `shouldBe` Just 10

    it "adds a new variable if it does not exist" $ do
      let result = execState (setVar "y" 5) (M.fromList [("x", 0)])
      M.lookup "y" result `shouldBe` Just 5

  describe "incVar" $ do
    it "increments the value of an existing variable" $ do
      let result = execState (incVar "x" 5) (M.fromList [("x", 10)])
      M.lookup "x" result `shouldBe` Just 15

  describe "getVar" $ do
    it "returns the value of an existing variable" $ do
      let result = evalState (getVar "x") (M.fromList [("x", 10)])
      result `shouldBe` 10

  describe "Fib" $ do
    it "returns correct result" $ do
      evalState (fib 0) M.empty `shouldBe` 1
      evalState (fib 1) M.empty `shouldBe` 1
      evalState (fib 10) M.empty `shouldBe` 89
      evalState (fib 20) (execState (fib 6) M.empty) `shouldBe` 10946

  describe "StateWithError" $ do
    it "getSt should return the current state" $ do
      let 
        (result, _) = runStateWithError getSt 5
      result `shouldBe` Right 5

    it "putSt should set the state to the given value" $ do
      let 
        (_, finalState) = runStateWithError (putSt 10) 5
      finalState `shouldBe` 10

    it "getStPlus1 should return the current state increased by 1" $ do
      let 
        (result, finalState) = runStateWithError getStPlus1 5
      result `shouldBe` Right 5
      finalState `shouldBe` 6

    it "getStPlus1IfNotTooBig should return the current state increased by 1 if it is not too big" $ do
      let 
        (result, finalState) = runStateWithError getStPlus1IfNotTooBig 5
      result `shouldBe` Right 5
      finalState `shouldBe` 6

    it "getStPlus1IfNotTooBig should terminate the computation with an error if the state is too big" $ do
      let 
        (result, _) = runStateWithError getStPlus1IfNotTooBig (11 :: Int)
      result `shouldBe` Left "Number is too big"


