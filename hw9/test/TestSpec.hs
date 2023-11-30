module TestSpec where

import           Test.Hspec
import           MyLib
import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict as M


spec :: Spec
spec = do
    -- Task 1
    describe "Firewall" $ do
        it "should match the correct rule" $ do
            let rules = [Rule Accept (IP 192 168 0 1) (IP 192 168 0 2), Rule Reject (IP 192 168 0 1) (IP 192 168 0 3)]
            let packet = Packet (IP 192 168 0 1) (IP 192 168 0 2)
            match rules packet `shouldBe` Just (Rule Accept (IP 192 168 0 1) (IP 192 168 0 2))

        it "should filter packets correctly" $ do
            let rules = [Rule Accept (IP 192 168 0 1) (IP 192 168 0 2), Rule Reject (IP 192 168 0 1) (IP 192 168 0 3)]
            let packet = Packet (IP 192 168 0 1) (IP 192 168 0 2)
            let (_, log') = runWriter $ filterOne rules packet
            log' `shouldBe` [Log 1 "Rule {action = Accept, source = IP 192 168 0 1, destination = IP 192 168 0 2}"]

        it "should merge log entries correctly" $ do
            let entries = [Log 1 "test", Log 1 "test", Log 1 "test2"]
            let (result, log') = runWriter $ mergeEntries [] entries
            result `shouldBe` [Log 1 "test2"]
            log' `shouldBe` [Log 2 "test"]

        it "should filter all packets correctly" $ do
            let rules = [Rule Accept (IP 192 168 0 1) (IP 192 168 0 2), Rule Reject (IP 192 168 0 1) (IP 192 168 0 3)]
            let packets = [Packet (IP 192 168 0 1) (IP 192 168 0 2), Packet (IP 192 168 0 1) (IP 192 168 0 3)]
            let (result, log') = runWriter $ filterAll rules packets
            result `shouldBe` [Packet (IP 192 168 0 1) (IP 192 168 0 2)]
            log' `shouldBe` [Log 1 "Rule {action = Accept, source = IP 192 168 0 1, destination = IP 192 168 0 2}", 
                             Log 1 "Rule {action = Reject, source = IP 192 168 0 1, destination = IP 192 168 0 3}"]

-- Task 2
    describe "lookupVar" $ do
        it "returns the value of a variable in the environment" $ do
            let env = Env {templs = M.empty, vars = M.singleton "var" "value"}
            lookupVar "var" env `shouldBe` Just "value"

        it "returns Nothing if the variable is not in the environment" $ do
            let env = Env {templs = M.empty, vars = M.empty}
            lookupVar "var" env `shouldBe` Nothing

    describe "lookupTemplate" $ do
        it "returns the template of a name in the environment" $ do
            let env = Env {templs = M.singleton "name" (Text "template"), vars = M.empty}
            lookupTemplate "name" env `shouldBe` Just (Text "template")

        it "returns Nothing if the name is not in the environment" $ do
            let env = Env {templs = M.empty, vars = M.empty}
            lookupTemplate "name" env `shouldBe` Nothing

    describe "addDefs" $ do
        it "adds new variables to the environment" $ do
            let env = Env {templs = M.empty, vars = M.empty}
            let newVars = M.singleton "var" "value"
            let newEnv = addDefs newVars env
            vars newEnv `shouldBe` newVars

    describe "resolveDef" $ do
        it "resolves a definition in the environment" $ do
            let env = Env {templs = M.singleton "name" (Text "template"), vars = M.empty}
            let def = Definition (Var (Text "name")) (Text "value")
            runReader (resolveDef def) env `shouldBe` ("name", "Text \"template\"")

    describe "getName" $ do
        it "gets the name of a Var template" $ do
            getName (Var (Text "name")) `shouldBe` "name"

        it "gets the name of a Text template" $ do
            getName (Text "name") `shouldBe` "name"

    describe "resolve" $ do
        it "resolves a Text template" $ do
            let env = Env {templs = M.empty, vars = M.empty}
            runReader (resolve (Text "text")) env `shouldBe` "text"

        it "resolves a Var template" $ do
            let env = Env {templs = M.empty, vars = M.singleton "var" "value"}
            runReader (resolve (Var (Text "var"))) env `shouldBe` "value"
-- Task4-4
    describe "fib" $ do
        it "returns 0 for input 0" $ do
            evalState (fib 0) (M.fromList [("prev", 0), ("cur", 1)]) `shouldBe` 0

        it "returns 1 for input 1" $ do
            evalState (fib 1) (M.fromList [("prev", 0), ("cur", 1)]) `shouldBe` 1

        it "returns 5 for input 5" $ do
            evalState (fib 5) (M.fromList [("prev", 0), ("cur", 1)]) `shouldBe` 5

  