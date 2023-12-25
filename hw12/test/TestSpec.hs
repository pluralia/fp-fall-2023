module TestSpec (spec) where

import           MyLib
import Data.Functor.Identity
import Control.Monad.State
import Test.Hspec
  (
    Spec
  , it
  , describe
  , shouldBe
  )

spec :: Spec
spec = do
    describe "Tests" $ do
      it "writeLog test" $ do
        runExample `shouldBe` (100,[(Info,"Starting the computation"),(Debug,"Intermediate result calculated"),(Info,"Computation completed")]) 
   
      it "loggingModification test" $ do
        let initState    = 1
            defState     = -99
            modifyState  = (* 2)
            isValidState = (> 0)
            Logged logs' newState = runIdentity $ runLoggerT $ evalStateT (loggingModification defState isValidState modifyState) initState
        newState `shouldBe` Just (2 :: Int)
        logs'    `shouldBe` [(Info,"Performing state modification"),(Info,"State modification complete"),(Info,"State satisfies the predicate")]

      it "modifyingLogging test" $ do
        let initState = 1
            defState = -99
            modifyState = (+ 1)
            isValidState = (> 0)
            (Logged logs' _, newState) = runIdentity . runStateT (runLoggerT $ modifyingLogging defState isValidState modifyState) $ initState
        newState `shouldBe` (2 :: Int)
        logs'    `shouldBe` [(Info,"Performing state modification"),(Info,"State modification complete"),(Info,"State satisfies the predicate")]

      it "modifyingLogging' test" $ do
        let initState = 1
            defState = -99
            modifyState = (+ 1)
            isValidState = (> 0)
            (Logged logs' _, newState) = runIdentity . runStateT (runLoggerT $ modifyingLogging defState isValidState modifyState) $ initState
        newState `shouldBe` (2 :: Int)
        logs'    `shouldBe` [(Info,"Performing state modification"),(Info,"State modification complete"),(Info,"State satisfies the predicate")]
