module TestSpec where

import MyLib
import Test.Hspec
import Control.Monad.Identity 
import Data.Char
import Control.Monad.State.Lazy

spec :: Spec
spec = do
    describe "Запись в лог" $ do
        it "writeLog" $ do

            let l1 = writeLog Debug "testing..." :: Logger ()
            let l2 = writeLog Info "writing some information..." :: Logger ()
            let l3 = writeLog Warning "Attention! New Year is very soon..." :: Logger ()
            let l4 = writeLog Error "You must good rest :)" :: Logger ()

            runIdentity (runLoggerT l1) `shouldBe` Logged {logs=[(Debug, "testing...")], val=()}
            runIdentity (runLoggerT l2) `shouldBe` Logged {logs=[(Info, "writing some information...")], val=()}
            runIdentity (runLoggerT l3) `shouldBe` Logged {logs=[(Warning, "Attention! New Year is very soon...")], val=()}
            runIdentity (runLoggerT l4) `shouldBe` Logged {logs=[(Error, "You must good rest :)")], val=()}
   
    describe "Теперь изменяем состояние!" $ do
        it "1. loggingModification" $ do

            let iniState1 = "lalala"
            let iniState2 = "2024 year"
            let m1 = loggingModification iniState1 (all isUpper) (map toUpper) :: StateT String (LoggerT Identity) (Maybe String)
            let m2 = loggingModification iniState2 (all isUpper) (map toUpper) :: StateT String (LoggerT Identity) (Maybe String)
            
            runIdentity (runLoggerT (runStateT m1 "Hellow")) `shouldBe` 
                Logged {logs=[(Info, "Return correct state.")], val=(Just "HELLOW", "HELLOW")}

            runIdentity (runLoggerT (runStateT m2 "234jkl")) `shouldBe` 
                Logged {logs=[(Info, "The state has been changed to default.")], val=(Nothing, iniState2)}

        it "2. modifyingLogging" $ do
            
            let iniState1 = "lalala"
            let iniState2 = "2024 year"
            let m3 = modifyingLogging iniState1 (all isUpper) (map toUpper) :: LoggerT (State String) ()
            let m4 = modifyingLogging iniState2 (all isUpper) (map toUpper) :: LoggerT (State String) ()
            
            runStateT (runLoggerT m3) "hellow" `shouldBe` 
                Identity (Logged {logs=[(Info, "Return correct state.")], val=()}, "HELLOW")
            runStateT (runLoggerT m4) "234jkl" `shouldBe` 
                Identity (Logged {logs=[(Info, "The state has been changed to default.")], val=()}, iniState2)

        it "3. modifyingLogging'" $ do
            
            let iniState1 = "lalala"
            let iniState2 = "2024 year"
            let m5 = modifyingLogging' iniState1 (all isUpper) (map toUpper) :: LoggerT (State String) ()
            let m6 = modifyingLogging' iniState2 (all isUpper) (map toUpper) :: LoggerT (State String) ()
            
            runStateT (runLoggerT m5) "hellow" `shouldBe` 
                Identity (Logged {logs=[(Info, "Return correct state.")], val=()}, "HELLOW")
            runStateT (runLoggerT m6) "234jkl" `shouldBe` 
                Identity (Logged {logs=[(Info, "The state has been changed to default.")], val=()}, iniState2)

