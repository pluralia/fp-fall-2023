module TestSpec where

import Test.Hspec
import MyLib
-- import Data.Bifunctor (Bifunctor (bimap))
import Data.Ix (inRange, index, range)

spec :: Spec
spec = do
  describe "Eq instance for ChurchNumber" $ do
    it "Equality of two Zeros" $ do
      Zero == Zero `shouldBe` True

    it "Equality of two Succ values" $ do
      (Succ (Succ Zero)) == (Succ (Succ Zero)) `shouldBe` True

    it "Inequality" $ do
      Zero == (Succ Zero) `shouldBe` False

  describe "Ord instance for ChurchNumber" $ do
    it "Comparison of two Zeros" $ do
      compare Zero Zero `shouldBe` EQ

    it "Comparison of Zero and Succ" $ do
      compare Zero (Succ Zero) `shouldBe` LT

    it "Comparison of Succ and Zero" $ do
      compare (Succ Zero) Zero `shouldBe` GT

  describe "Num instance for ChurchNumber" $ do
    it "Addition of Zero and Succ" $ do
      (Zero + Succ (Succ Zero)) `shouldBe` (Succ (Succ Zero))
      
    it "Subtraction of Succ and Zero" $ do
      ((Succ (Succ Zero)) - Zero) `shouldBe` (Succ (Succ Zero))

    it "Multiplication of Succ values" $ do
      (Succ (Succ Zero) * Succ (Succ Zero)) `shouldBe` Succ (Succ (Succ (Succ (Zero))))

  describe "Ix instance for ChurchNumber" $ do
    it "Range from Zero to Zero" $ do
      range (Zero, Zero) `shouldBe` [Zero]

    it "Index Zero in the range" $ do
      index (Zero, Succ (Succ Zero)) Zero `shouldBe` 0

    it "In range Zero" $ do
      inRange (Zero, Succ (Succ Zero)) Zero `shouldBe` True

    it "Not in range Succ" $ do
      inRange (Zero, Succ Zero) (Succ (Succ Zero)) `shouldBe` False

  -- describe "InOrder instance for Tree" $ do
  --   it "In-Order traversal of a single node tree" $ do
  --       let tree = In (Node 12345 [])
  --       show tree `shouldBe` "12345"

  --   it "In-Order traversal of a tree with multiple nodes" $ do
  --       let tree = In (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])
  --       show tree `shouldBe` "24531"
  
  -- describe "PreOrder instance for Tree" $ do
  --   it "Pre-Order traversal of a single-node tree" $ do
  --       let tree = Pre (Node 12345 [])
  --       show tree `shouldBe` "12345"

  --   it "Pre-Order traversal of a tree with multiple nodes" $ do
  --       let tree = Pre (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])
  --       show tree `shouldBe` "12345"
  
  -- describe "PostOrder instance for Tree" $ do
  --   it "Post-Order traversal of a single-node tree" $ do
  --       let tree = Post (Node 12345 [])
  --       show tree `shouldBe` "12345"

  --   it "Post-Order traversal of a tree with multiple nodes" $ do
  --       let tree = Post (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])
  --       show tree `shouldBe` "24531"

  -- describe "ToCMYK for [Int] format" $ do
  --   it "All zeros input" $ do
  --     let input  = ([0, 0, 0, 0] :: [Int])
  --     let output = Just (UnsafeMkCMYK {cyan = 0, magenta = 0, yellow = 0, black = 0})
  --     toCMYK input `shouldBe` output

  --   it "All 100 input" $ do
  --     let input  = ([100, 100, 100, 100] :: [Int])
  --     let output = Just (UnsafeMkCMYK {cyan = 100, magenta = 100, yellow = 100, black = 100})
  --     toCMYK input `shouldBe` output

  --   it "Wrong input with out of range number" $ do
  --     let input  = ([200, 100, 100, 100] :: [Int])
  --     let output = Nothing
  --     toCMYK input `shouldBe` output

  -- describe "ToCMYK for RGB format" $ do
  --   it "All zeros input" $ do
  --     let input  = (UnsafeMkRGB 0 0 0)
  --     let output = Just (UnsafeMkCMYK {cyan = 0, magenta = 0, yellow = 0, black = 100})
  --     toCMYK input `shouldBe` output

  --   it "All 100 input" $ do
  --     let input  = (UnsafeMkRGB 100 100 100)
  --     let output = Just (UnsafeMkCMYK {cyan = 0, magenta = 0, yellow = 0, black = 61})
  --     toCMYK input `shouldBe` output

  --   it "Wrong input with out of range number" $ do
  --     let input  = (UnsafeMkRGB 200 25 1)
  --     let output = Nothing
  --     toCMYK input `shouldBe` output
    
  -- describe "dToSMYK for [Int]" $ do
  --   it "All zeros input" $ do
  --     let output = Just (UnsafeMkCMYK {cyan = 0, magenta = 0, yellow = 0, black = 100})
  --     toCMYK' dToCMYKList [0, 0, 0] `shouldBe` output

  --   it "All 100 input" $ do
  --     let output = Just (UnsafeMkCMYK {cyan = 0, magenta = 0, yellow = 0, black = 61})
  --     toCMYK' dToCMYKList [100, 100, 100] `shouldBe` output

  --   it "Wrong input with out of range number" $ do
  --     let output = Nothing
  --     toCMYK' dToCMYKList [200, 100, 100] `shouldBe` output 

  -- describe "dToCMYK for RGB format" $ do
  --   it "All zeros input" $ do
  --     let output = Just (UnsafeMkCMYK {cyan = 0, magenta = 0, yellow = 0, black = 100})
  --     toCMYK' dToCMYKRGB (UnsafeMkRGB 0 0 0) `shouldBe` output

  --   it "All 100 input" $ do
  --     let output = Just (UnsafeMkCMYK {cyan = 0, magenta = 0, yellow = 0, black = 61})
  --     toCMYK' dToCMYKRGB (UnsafeMkRGB 100 100 100) `shouldBe` output

  --   it "Wrong input with out of range number" $ do
  --     let output = Nothing
  --     toCMYK' dToCMYKRGB (UnsafeMkRGB 5000 100 100) `shouldBe` output

  -- describe "nextDay" $ do
  --   it "Monday -> Tuesday" $ do
  --       nextDay Monday `shouldBe` Tuesday

  --   it "Saturday -> Sunday" $ do
  --       nextDay Saturday `shouldBe` Sunday
      
  -- describe "dayBefore" $ do
  --   it "Monday <- Tuesday" $ do
  --       dayBefore Tuesday `shouldBe` Monday

  --   it "Saturday <- Sunday" $ do
  --       dayBefore Sunday `shouldBe` Saturday
      
  -- describe "daysBeforeWeekend" $ do
  --   it "Whole working week OMG why so long i want to rest" $ do
  --       daysBeforeWeekend Monday `shouldBe` 5

  --   it "ITS WEEKENDS!!!" $ do
  --       daysBeforeWeekend Saturday `shouldBe` 0

  -- describe "Functor instance for List" $ do
  --   it "Functor save structure" $ do
  --       fmap id (Cons 1 (Cons 2 Nil)) `shouldBe` (Cons 1 (Cons 2 Nil))

  --   it "Functor applies function" $ do
  --       fmap (*10) (Cons 1 (Cons 2 Nil)) `shouldBe` (Cons 10 (Cons 20 Nil))

  -- describe "Functor instance for Tree" $ do
  --   it "Functor save structure" $ do
  --       let tree = Node 1 [Node 2 [], Node 3 [Node 4 []]]
  --       fmap id tree `shouldBe` tree

  --   it "Functor applies function" $ do
  --       let tree = Node 1 [Node 2 [], Node 3 [Node 4 []]]
  --       let expectedTree = Node 10 [Node 20 [], Node 30 [Node 40 []]]
  --       fmap (*10) tree `shouldBe` expectedTree

  -- describe "Functor instance for Pair" $ do
  --   it "Functor applies function to list" $ do
  --     fmap (/ 2) (Pair (1 :: Int) ([2, 4, 6, 8, 10] :: [Int])) `shouldBe` Pair (1 :: Int) ([1, 2, 3, 4, 5] :: [Int])
    
  --   it "Functor applies function to the second element of the Pair" $ do
  --     fmap length (Pair 1 "RickAndMorty") `shouldBe` (Pair 1 12)

  -- describe "biFunctor" $ do
  --   it "biFunctor applies function to Either'" $ do
  --     bimap (++ "in and out") (Left' "5 min adventure, ") `shouldBe` (Left' "5 min adventure, in and out")

  --   it "biFunctor applies function to the Left'" $ do
  --     bimap ((+ 7) :: Int -> Int) ((++ "NOPE") :: String -> String) Left' (3 :: Int) `shouldBe` Left' (10 :: Int)

  --   it "biFunctor applies function to Pair" $ do
  --     let input  = Pair (5 :: Int) (55 :: Int)
  --     let output = Pair (10 :: Int) (11 :: Int)
  --     bimap ((* 2) :: Int -> Int) ((/ 5) :: Int -> Int) input `shouldBe` output

  describe "VibeCheck instance" $ do
    it "Best person ever" $ do
      checker "Introvert" `shouldBe` "Nice, u already perfect"

    it "How do u talk so much???" $ do
      checker "Extravert" `shouldBe` "Well, I guess someone should be this talkative"
