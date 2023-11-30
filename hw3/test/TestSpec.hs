module TestSpec where


 import Test.Hspec
   (
     Spec
   , it
   , shouldBe
   , describe
   , shouldNotBe
   )
 import MyLib
 import Data.Ix (range, index, inRange)
 import Data.Bifunctor ( Bifunctor(bimap) ) 

 spec :: Spec
 spec = do
      describe "Eq instance Church" $ do
        it "Checks equality of Zero with Zero" $
          Zero `shouldBe` (Zero :: ChurchNumber)

        it "Checks equality of Succ Zero with Zero" $
          Succ Zero `shouldBe` (Succ Zero :: ChurchNumber)

        it "Checks inequality of Zero with Succ Zero" $
          Zero `shouldNotBe` (Succ Zero :: ChurchNumber)

        it "Checks inequality of Succ Zero with Succ (Succ Zero)" $
          Succ Zero `shouldNotBe` (Succ (Succ Zero) :: ChurchNumber)

      describe "Ord instance for Church" $ do
        it "Compares Zero with Zero" $
          compare Zero Zero `shouldBe` EQ

        it "Compares Zero with Succ Zero" $
          compare Zero (Succ Zero) `shouldBe` LT

        it "Compares Succ Zero with Zero" $
          compare (Succ Zero) Zero `shouldBe` GT

      describe "Num instance for Church" $ do
        it "Adds Zero to a Church number" $
          (Zero + Succ Zero) `shouldBe` (Succ Zero :: ChurchNumber)

        it "Subtracts a Church number from Zero" $
          (Zero - Succ Zero) `shouldBe` (Zero :: ChurchNumber)

        it "Multiplies Zero by a Church number" $
          (Zero * Succ (Succ Zero)) `shouldBe` (Zero :: ChurchNumber)

        it "Checks 'fromInteger' for a non-zero number" $
          fromInteger 4 `shouldBe` (Succ (Succ (Succ (Succ Zero))))

        it "Checks 'fromInteger' for zero" $
          fromInteger 0 `shouldBe` (Zero :: ChurchNumber)

        it "Checks 'abs' function for Zero" $
          abs Zero `shouldBe` (Zero :: ChurchNumber)

        it "Adds two Church numbers" $
          (Succ Zero + Succ Zero) `shouldBe` (Succ (Succ Zero) :: ChurchNumber)
        
      describe "Ix instance for Church" $ do
        it "Checks the range of Church numbers" $
          range (Zero, Succ (Succ Zero)) `shouldBe` [Zero, Succ Zero, Succ (Succ Zero)]

        it "Checks if a Church number is in range" $
          inRange (Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` True

        it "Indexes a Church number" $
          index (Zero, Succ (Succ Zero)) (Succ (Succ (Succ Zero)))   `shouldBe` 3

        it "Checks inRange for a number outside the range" $
          inRange (Zero, Succ (Succ Zero)) (Succ (Succ (Succ Zero))) `shouldBe` False
        

      describe "Enum instance for Day" $ do
        it "Converts Tue to its enum value" $
          fromEnum Tuesday `shouldBe` 2
        it "Converts Fri to its enum value" $
          fromEnum Friday `shouldBe` 5
        it "Converts enum value 4 to Thu" $
          toEnum 4 `shouldBe` (Thursday :: Day)
      
      describe "dayBefore function" $ do
        it "from Tue to Mon" $
          dayBefore Tuesday `shouldBe` Monday
        it "Returns Sat for Fri" $
          dayBefore Saturday `shouldBe` Friday
        it "Returns Fri for Thue" $
          dayBefore Friday `shouldBe` Thursday


      describe "daysBeforeWeekend" $ do
        it "Returns 5 for Monday" $
          daysBeforeWeekend Wednesday `shouldBe` 3
        it "Returns 3 for Thursday" $
          daysBeforeWeekend Thursday `shouldBe` 2
        it "Returns " $
          daysBeforeWeekend Sunday `shouldBe` 6
      
      describe "Functor instance for List" $ do
        it "Maps a function over an empty list" $
          fmap (+ 1) (Nil :: List Integer) `shouldBe` (Nil :: List Integer)
        it "Maps a function over a non-empty list" $
          fmap (*3) (Cons 4 (Cons 7 Nil)) `shouldBe` (Cons 12 (Cons 21 Nil) :: List Int)
      
      describe "Functor instance for Tree" $ do
        it "Maps a function over a tree with a single node" $
          fmap (*2) (Node 5 []) `shouldBe` (Node 10 [] :: Tree Int)
        it "Maps a function over a tree with multiple nodes and branches" $
          fmap (+1) (Node 10 [Node 20 [], Node 30 [Node 40 [], Node 50 []]])
          `shouldBe`
          (Node 11 [Node 21 [], Node 31 [Node 41 [], Node 51 []]] :: Tree Int)

      describe "Functor instance for Pair" $ do
        it "Maps a function over the second element of a Pair" $
          fmap (+1) (Pair "Buzz" 5) `shouldBe` (Pair "Buzz" 6 :: Pair String Int)
        it "Maps a function that multiplies each element of the second list by 10" $
          fmap (map (*3)) (Pair "Extra" [1, 2, 3]) `shouldBe`
          (Pair "Extra" [3, 6, 9] :: Pair String [Int])
      
      describe "Bifunctor instance for Either'" $ do
        it "Maps a function over the Left side of Either'" $
          bimap (+10) (*2) (Left' 5) `shouldBe` (Left' 15 :: Either' Int Int)
        it "Maps a function over the Right side of Either'" $
          bimap (*2) (+10) (Right' 7) `shouldBe` (Right' 17 :: Either' Int Int)
      





      

