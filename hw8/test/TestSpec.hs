module TestSpec (spec) where

import Test.Hspec
  (
    Spec
  , it
  , shouldBe
  , describe
  )

import MyLib


spec :: Spec
spec = do
  describe "simple parsers" $ do
    it "Fasta" $ do
      
