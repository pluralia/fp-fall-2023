{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import MyLib
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  -- for tests
  let one = Succ Zero
  -- HW5
  describe "Eq ChurchNumber'" $ do
    it "go" $ do
      one == three `shouldBe` False
      one == four `shouldNotBe` True
      one /= three `shouldBe` True
      one == one `shouldBe` True