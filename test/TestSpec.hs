module TestSpec (spec) where

import MyLib
import Test.Hspec
import Control.Monad.State
import Data.Functor.Identity

spec :: Spec
spec = do
    describe "2. Реализуйте функцию, позволяющую записать что-то в лог внутри трансформера 'LoggerT'" $ do
        it "creates a log entry with the given log level and message" $ do
            let logLevel = Info
                message = "Test message"
                Logged logss logged = runIdentity (runLoggerT (writeLog logLevel message))
            logss `shouldBe` [(logLevel, message)]
            logged `shouldBe` ()

        it "contains exactly one message in the logs" $ do
            let logLevel = Warning
                message = "This is a warning"
                Logged logss _ = runIdentity (runLoggerT (writeLog logLevel message))
            length logss `shouldBe` 1


    describe "3. Реализуйте функцию `loggingModification`" $ do
        it "modifies state and returns Just new state when predicate is true" $ do
            let originalState = 0
                defState = -1
                testFunc = (+ 1)
                testPredicate = (> 0)
                loggedResult = runIdentity $ runLoggerT $ evalStateT (loggingModification defState testPredicate testFunc) originalState
                Logged logss newState = loggedResult
            newState `shouldBe` (Just (1 :: Int))
            logss `shouldContain` [(Info, "State modified")]

        it "replaces state with default when predicate is false and returns Nothing" $ do
            let originalState = 0
                defState = -1
                testFunc = (+ 1)
                testPredicate = (const False)
                loggedResult = runIdentity $ runLoggerT $ evalStateT (loggingModification defState testPredicate testFunc) originalState
                Logged logss newState = loggedResult
            newState `shouldBe` (Nothing :: Maybe Int)
            logss `shouldContain` [(Info, "State replaced with default: -1")]


    describe "4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'" $ do
        it "modifies state and logs 'State modified' and 'State passes the predicate' when predicate is true" $ do
            let originalState = 0
                defState = -1
                testFunc = (+ 1)
                testPredicate = (> 0)
                loggerAction = modifyingLogging defState testPredicate testFunc
                loggedResult = runIdentity . runStateT (runLoggerT loggerAction) $ originalState
                (Logged logss _, finalState) = loggedResult
            finalState `shouldBe` (1 :: Int)
            logss `shouldContain` [(Info,"State modified"),(Info,"State read: 1"),(Info,"State passes the predicate: 1")]

        it "replaces state with default, logs 'State modified' and 'State replaced with default' when predicate is false" $ do
            let originalState = 0
                defState = -1 :: Int
                testFunc = const 2
                testPredicate = (< 0)
                loggerAction = modifyingLogging defState testPredicate testFunc
                loggedResult = runIdentity . runStateT (runLoggerT loggerAction) $ originalState
                (Logged logss _, finalState) = loggedResult
            finalState `shouldBe` defState
            logss `shouldContain` [(Info,"State modified"),(Info,"State read: 2"),(Info,"State replaced with default: -1")]

