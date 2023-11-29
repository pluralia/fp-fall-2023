module TestSpec (spec) where

import qualified Data.Map.Strict as M
import           Data.Functor.Identity
import           Control.Monad.Writer.Strict
import           Control.Monad.Reader
import           Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

import MyLib

spec :: Spec
spec = do
  describe "traversable" $ do
    it "Maybe'" $ do
      let 
        f :: Int -> [Int]
        f x = [x, x, x]
      traverse f (Just' 1 :: Maybe' Int) `shouldBe` ([Just' 1, Just' 1, Just' 1] :: [Maybe' Int])
      sequenceA (Just' [1, 2, 3] :: Maybe' [Int]) `shouldBe` ([Just' 1, Just' 2, Just' 3] :: [Maybe' Int])
      sequenceA [Just' 1, Just' 2, Just' 3] `shouldBe` (Just' [1, 2, 3] :: Maybe' [Int])