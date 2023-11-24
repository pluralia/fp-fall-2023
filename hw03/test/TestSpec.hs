module TestSpec where

import MyLib
import Test.Hspec (Spec, it, shouldBe, describe, shouldNotBe, shouldMatchList)
import Data.Ix (range, index, inRange)
import Data.Bifunctor ( Bifunctor(bimap) ) 

import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Data.Map.Strict as M

spec :: Spec
spec = do
  describe "HW3: Eq instance for ChurchNumber" $ do
    it "Checks equality of Zero with Zero" $
      Zero `shouldBe` (Zero :: ChurchNumber)
    it "Checks equality of Succ Zero with Zero" $
      Succ Zero `shouldBe` (Succ Zero :: ChurchNumber)
    it "Checks inequality of Zero with Succ Zero" $
      Zero `shouldNotBe` (Succ Zero :: ChurchNumber)

  describe "HW3: Ord instance for ChurchNumber" $ do
    it "Compares Zero with Zero" $
      compare Zero Zero `shouldBe` EQ
    it "Compares Zero with Succ Zero" $
      compare Zero (Succ Zero) `shouldBe` LT
    it "Compares Succ Zero with Zero" $
      compare (Succ Zero) Zero `shouldBe` GT

  describe "HW3: Num instance for ChurchNumber" $ do
    it "Adds two Church numbers" $
      (Succ Zero + Succ Zero) `shouldBe` (Succ (Succ Zero) :: ChurchNumber)
    it "Subtracts two Church numbers" $
      (Succ (Succ Zero) - Succ Zero) `shouldBe` (Succ Zero :: ChurchNumber)
    it "Multiplies two Church numbers" $
      (Succ (Succ Zero) * Succ (Succ Zero)) `shouldBe` (Succ (Succ (Succ (Succ Zero))) :: ChurchNumber)
    it "Checks 'abs' function for Zero" $
      abs Zero `shouldBe` (Zero :: ChurchNumber)
    it "Checks 'abs' function for non Zero" $
      abs (Succ (Succ Zero)) `shouldBe` (Succ (Succ Zero) :: ChurchNumber)
    it "Checks 'signum' function for a non-zero number" $
      signum (Succ (Succ Zero)) `shouldBe` (Succ Zero :: ChurchNumber)

  describe "HW3: Ix instance for ChurchNumber" $ do
    it "Checks the range of Church numbers" $
      range (Zero, Succ (Succ Zero)) `shouldBe` [Zero, Succ Zero, Succ (Succ Zero)]
    it "Checks if a Church number is in range" $
      inRange (Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` True
    it "Indexes a Church number" $
      index (Zero, Succ (Succ Zero)) (Succ (Succ Zero)) `shouldBe` 2
    it "Checks inRange for a number outside the range" $
      inRange (Zero, Succ (Succ Zero)) (Succ (Succ (Succ Zero))) `shouldBe` False
    it "Checks index for a number outside the range" $
      index (Zero, Succ (Succ Zero)) (Succ (Succ (Succ Zero))) `shouldBe` 3

  describe "HW3: Enum instance for Day" $ do
    it "Converts Monday to its enum value" $
      fromEnum Monday `shouldBe` 0
    it "Converts Saturday to its enum value" $
      fromEnum Saturday `shouldBe` 5
    it "Converts 4 to its corresponding Day value" $
      (toEnum 4 :: Day) `shouldBe` Friday

  describe "HW3: nextDay function" $ do
    it "Returns Tuesday for Monday" $
      nextDay Monday `shouldBe` Tuesday
    it "Returns Sunday for Saturday" $
      nextDay Saturday `shouldBe` Sunday
    it "Returns Monday for Sunday" $
      nextDay Sunday `shouldBe` Monday

  describe "HW3: dayBefore function" $ do
    it "Returns Sunday for Monday" $
      dayBefore Monday `shouldBe` Sunday
    it "Returns Saturday for Sunday" $
      dayBefore Sunday `shouldBe` Saturday
    it "Returns Tuesday for Wednesday" $
      dayBefore Wednesday `shouldBe` Tuesday

  describe "HW3: daysBeforeWeekend function" $ do
    it "Returns 5 for Monday" $
      daysBeforeWeekend Monday `shouldBe` 5
    it "Returns 0 for Saturday" $
      daysBeforeWeekend Saturday `shouldBe` 0
    it "Returns 3 for Thursday" $
      daysBeforeWeekend Thursday `shouldBe` 2

  describe "HW3: Functor instance for List" $ do
    it "Maps a function over an empty list" $
      fmap (+1) Nil `shouldBe` (Nil :: List Int)
    it "Maps a function over a non-empty list" $
      fmap (*2) (Cons 3 (Cons 5 Nil)) `shouldBe` (Cons 6 (Cons 10 Nil) :: List Int)
    it "Maps a function that turns values to strings" $
      fmap show (Cons (42 :: Int) Nil) `shouldBe` (Cons "42" Nil :: List String)
  
  describe "HW3: Functor instance for Tree" $ do
    it "Maps a function over a tree with one node" $
      (fmap (*2) (Node 5 []) :: Tree Int) `shouldBe` (Node 10 [] :: Tree Int)
    it "Maps a function over a tree with multiple levels 1" $
      fmap show (Node (42 :: Int)  [Node (17 :: Int) [], Node (99 :: Int) [Node (7 :: Int) []]]) `shouldBe`
        (Node "42" [Node "17" [], Node "99" [Node "7" []]] :: Tree String)
    it "Maps a function over a tree with multiple levels 2" $
      fmap (*2) (Node (42 :: Int)  [Node (17 :: Int) [], Node (99 :: Int) [Node (7 :: Int) []]]) `shouldBe`
        (Node 84 [Node 34 [], Node 198 [Node 14 []]] :: Tree Int)

  describe "HW3: Functor instance for Pair" $ do
    it "Maps a function over the second element of the pair" $
      fmap (*2) (Pair "Hello" 5) `shouldBe` (Pair "Hello" 10 :: Pair String Int)
    it "Maps a function that multiplies each element of the second list by 10" $
      fmap (map (*10)) (Pair "hello" [1, 2, 3]) `shouldBe`
        (Pair "hello" [10, 20, 30] :: Pair String [Int])

  describe "HW3: Bifunctor instance for Either'" $ do
    it "Maps a function over the Left side of Either'" $
      bimap (+10) (*2) (Left' 5) `shouldBe` (Left' 15 :: Either' Int Int)
    it "Maps a function over the Right side of Either'" $
      bimap (*2) (+10) (Right' 7) `shouldBe` (Right' 17 :: Either' Int Int)

  describe "HW3: Bifunctor instance for Pair" $ do
    it "Maps a function over the first element of the Pair" $
      bimap (*2) length (Pair 5 "hello") `shouldBe` (Pair 10 5 :: Pair Int Int)
    it "Maps a function over the second element of the Pair" $
      bimap (++ " world!") length (Pair "Hello" ([1, 2, 3] :: [Int])) `shouldBe`
        (Pair "Hello world!" 3 :: Pair String Int)

  describe "HW4: padZero" $ do
    it "handles empty string" $ do
      padZero T.empty 5 `shouldBe` T.pack "00000"
    it "handles string larger than width" $ do
      padZero (T.pack "12345") 3 `shouldBe` T.pack "12345"

  describe "HW4: evenodd" $ do
    it "handles empty list" $ do
      evenodd ([] :: [Int]) `shouldBe` ([], [])
    it "handles list with odd and even elements" $ do
      evenodd [1, 2, 3, 4, 5 :: Int] `shouldBe` ([1, 3, 5 :: Int], [2, 4 :: Int])

  describe "HW4: average" $ do
    it "handles empty vector" $ do
      average (V.empty :: V.Vector Double) `shouldBe` 0
    it "handles vector with values" $ do
      average (V.fromList [1, 2, 3, 4, 5]) `shouldBe` 3.0

  describe "HW4: gcContent" $ do
    it "handles empty string" $ do
      gcContent T.empty `shouldBe` 0
    it "handles DNA string" $ do
      gcContent (T.pack "ATCGATCGATCG") `shouldBe` 0.5

  describe "HW4: fromListL and fromListR" $ do
    let lst = [(1, "one"), (2, "two"), (2, "three")]
    it "fromListL works as expected" $ do
      fromListL lst `shouldBe` M.fromList [(1 :: Int, "one"), (2 :: Int, "three")]
    it "fromListR works as expected" $ do
      fromListR lst `shouldBe` M.fromList [(1 :: Int, "one"), (2 :: Int, "two")]
    let lst' = [(1, "one"), (2, "two"), (2, "three"), (2, "four")]
    it "fromListL handles duplicates correctly" $ do
      fromListL lst' `shouldBe` M.fromList [(1 :: Int, "one"), (2 :: Int, "four")]
    it "fromListR handles duplicates correctly" $ do
      fromListR lst' `shouldBe` M.fromList [(1 :: Int, "one"), (2 :: Int, "two")]

  describe "HW4: nubOrd" $ do
    it "handles empty list" $ do
      nubOrd ([] :: [Int]) `shouldBe` []
    it "handles list with duplicate elements" $ do
      nubOrd [1, 2, 1, 3, 2, 4 :: Int] `shouldMatchList` [1, 2, 3, 4 :: Int]

  describe "HW4: buildQuery" $ do
    let queryParams = M.fromList [(T.pack "a", T.pack "1"), (T.pack "b", T.pack "2"), (T.pack "c", T.pack "hello")]
    it "builds query parameters correctly" $ do
      buildQuery queryParams `shouldBe` T.pack "a=1&b=2&c=hello"
    it "returns an empty string for empty parameters" $ do
      buildQuery M.empty `shouldBe` T.empty
