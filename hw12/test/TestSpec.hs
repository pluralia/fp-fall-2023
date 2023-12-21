module TestSpec where

import Test.Hspec
import MyLib
import Control.Monad.Identity
import Control.Monad.State    
import Control.Monad.Identity (Identity)
import Control.Monad.Trans    (MonadTrans (..))
import Prelude hiding         (log)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad

spec :: Spec
spec = do
    describe "LoggerT Functor instance" $ do
        it "should apply function to value inside LoggerT" $ do
            let logger = LoggerT . Identity $ Logged [(Info, "Initial log")] 5
            let result = fmap ((+1) :: Int -> Int) logger
            runIdentity (runLoggerT result) `shouldBe` Logged [(Info, "Initial log")] 6

    describe "LoggerT Applicative instance" $ do
        it "should apply function inside LoggerT to value inside LoggerT" $ do
            let loggerFunc = LoggerT . Identity $ Logged [(Info, "Function log")] ((+1) :: Int -> Int)
            let loggerVal = LoggerT . Identity $ Logged [(Info, "Value log")] 5
            let result = loggerFunc <*> loggerVal
            runIdentity (runLoggerT result) `shouldBe` Logged [(Info, "Function log"), (Info, "Value log")] 6

