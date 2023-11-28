module TestSpec where

import Test.Hspec
import MyLib
import           Control.Monad.Writer.Lazy
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import qualified System.Random as R
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "1. Writer: Брандмауэр" $ do
        it "matchesRule returns True for matching rule" $ do
            let packet = Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            let rule = Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            matchesRule packet rule `shouldBe` True

        it "matchesRule returns False for non-matching rule" $ do
            let packet = Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            let rule = Rule Reject (IPAddress "192.168.1.1") (IPAddress "192.168.2.3")
            matchesRule packet rule `shouldBe` False

        it "match returns Just Rule for matching rule" $ do
            let packet = Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            let rules = [Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")]
            match rules packet `shouldBe` Just (head rules)

        it "match returns Nothing for non-matching rule" $ do
            let packet = Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            let rules = [Rule Reject (IPAddress "192.168.1.1") (IPAddress "192.168.2.3")]
            match rules packet `shouldBe` Nothing

        it "filterOne returns Just Packet for accepted packet" $ do
            let packet = Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            let rules = [Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")]
            snd (runWriter (filterOne rules packet)) `shouldBe` [Log 1 "Packet from IPAddress \"192.168.1.1\" to IPAddress \"192.168.2.2\" was accepted"]

        it "filterOne returns Nothing for rejected packet" $ do
            let packet = Packet (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")
            let rules = [Rule Reject (IPAddress "192.168.1.1") (IPAddress "192.168.2.2")]
            snd (runWriter (filterOne rules packet)) `shouldBe` [Log 1 "Packet from IPAddress \"192.168.1.1\" to IPAddress \"192.168.2.2\" was rejected"]

        it "mergeEntries merges logs with identical messages" $ do
            let log1 = [Log 2 "Error"]
            let log2 = [Log 3 "Error"]
            snd (runWriter (mergeEntries log1 log2)) `shouldBe` [Log 5 "Error"]

        it "mergeEntries keeps logs with different messages separate" $ do
            let log1 = [Log 2 "Error1"]
            let log2 = [Log 3 "Error2"]
            snd (runWriter (mergeEntries log1 log2)) `shouldBe` [Log 2 "Error1", Log 3 "Error2"]

        it "filterAll returns an empty list if no packets are provided" $ do
            let rules = [Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.1.2")]
            snd (runWriter (filterAll rules [])) `shouldBe` []

        it "filterAll returns filtered packets along with logs" $ do
            let rules = [Rule Accept (IPAddress "192.168.1.1") (IPAddress "192.168.1.2")]
            let packets = [Packet (IPAddress "192.168.1.1") (IPAddress "192.168.1.2"), Packet (IPAddress "192.168.1.3") (IPAddress "192.168.1.4")]
            let expectedResult = [Packet (IPAddress "192.168.1.1") (IPAddress "192.168.1.2")]
            let (_, logs) = runWriter (filterAll rules packets)
            length logs `shouldBe` 2 
            length (fst (runWriter (filterAll rules packets))) `shouldBe` 1 
            fst (runWriter (filterAll rules packets)) `shouldBe` expectedResult 

    describe "2. Reader: Инстанцирование шаблонов" $ do
        it "lookupVar returns Nothing for an empty environment" $ do
            let env = Env { templs = M.empty, vars = M.empty }
            lookupVar "testVar" env `shouldBe` Nothing

        it "lookupVar returns the value for an existing variable in the environment" $ do
            let env = Env { templs = M.empty, vars = M.fromList [("Mikky", "Mouse")] }
            lookupVar "Mikky" env `shouldBe` Just "Mouse"

        it "lookupVar returns Nothing for a non-existing variable in the environment" $ do
            let env = Env { templs = M.empty, vars = M.fromList [("Skubby", "Doo")] }
            lookupVar "Mikky" env `shouldBe` Nothing

        it "lookupTemplate returns Nothing for an empty environment" $ do
            let env = Env { templs = M.empty, vars = M.empty }
            lookupTemplate "testTemplate" env `shouldBe` Nothing

        it "lookupTemplate returns the template for an existing template name in the environment" $ do
            let template = Text "Test content"
                env = Env { templs = M.singleton "testTemplate" template, vars = M.empty }
            lookupTemplate "testTemplate" env `shouldBe` Just template

        it "lookupTemplate returns Nothing for a non-existing template in the environment" $ do
            let template = Text "Another content"
                env = Env { templs = M.singleton "anotherTemplate" template, vars = M.empty }
            lookupTemplate "Darth Vader" env `shouldBe` Nothing

        it "addDefs adds new variables to an empty environment" $ do
            let env = Env { templs = M.empty, vars = M.empty }
                newVars = M.fromList [("var1", "value1"), ("var2", "value2")]
                updatedEnv = addDefs newVars env
            vars updatedEnv `shouldBe` newVars

        it "addDefs adds new variables to an existing environment" $ do
            let existingVars = M.fromList [("existingVar", "existingValue")]
                env = Env { templs = M.empty, vars = existingVars }
                newVars = M.fromList [("var1", "value1"), ("var2", "value2")]
                updatedEnv = addDefs newVars env
                expectedVars = M.union existingVars newVars
            vars updatedEnv `shouldBe` expectedVars

    describe "3. State: Генерация случайного значения кастомного типа" $ do
        it "getAny returns Int when given Int" $ do
            let (value, _) = runState getAny (R.mkStdGen 42 :: R.StdGen)
            let intResult = runState (getAny :: State R.StdGen Int) (R.mkStdGen 42)
            value `shouldBe` fst intResult

        it "getAny returns Char when given Char" $ do
            let (value, _) = runState getAny (R.mkStdGen 42 :: R.StdGen)
            let charResult = runState (getAny :: State R.StdGen Char) (R.mkStdGen 42)
            value `shouldBe` fst charResult

        it "getOne returns Int when given Int bounds" $ do
            let intBounds = (1, 10) :: (Int, Int)
            let (value, _) = runState (getOne intBounds) (R.mkStdGen 42 :: R.StdGen)
            value `shouldSatisfy` (\x -> x >= fst intBounds && x <= snd intBounds)

        it "getOne returns Char when given Char bounds" $ do
            let charBounds = ('a', 'z') :: (Char, Char)
            let (value, _) = runState (getOne charBounds) (R.mkStdGen 42 :: R.StdGen)
            value `shouldSatisfy` (\x -> x >= fst charBounds && x <= snd charBounds)

    describe "4. 'Императивное' программирование" $ do
        it "setVar updates an existing variable value" $ do
            let initialState = M.fromList [("x", 10), ("y", 20)]
            let updatedState = execState (setVar "x" 15) initialState
            M.lookup "x" updatedState `shouldBe` Just 15

        it "setVar adds a new variable to the context" $ do
            let initialState = M.fromList [("x", 10), ("y", 20)]
            let updatedState = execState (setVar "z" 30) initialState
            M.lookup "z" updatedState `shouldBe` Just 30

        it "incVar increments an existing variable value" $ do
            let initialState = M.fromList [("x", 10), ("y", 20)]
            let updatedState = execState (incVar "x" 5) initialState
            M.lookup "x" updatedState `shouldBe` Just 15

        it "incVar throws an error when trying to increment a non-existing variable" $ do
            let initialState = M.fromList [("x", 10), ("y", 20)]
            evaluate (execState (incVar "z" 5) initialState) `shouldThrow` anyErrorCall

        it "getVar retrieves an existing variable value" $ do
            let initialState = M.fromList [("x", 10), ("y", 20)]
            evalState (getVar "x") initialState `shouldBe` 10

        it "getVar throws an error when trying to retrieve a non-existing variable" $ do
            let initialState = M.fromList [("x", 10), ("y", 20)]
            evaluate (evalState (getVar "z") initialState) `shouldThrow` anyErrorCall

