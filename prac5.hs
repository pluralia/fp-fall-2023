{-# LANGUAGE InstanceSigs #-}

import Data.Monoid
import Data.Bifunctor

-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- newtype Endo b = Endo { appEndo :: b -> b }

-- foldMap ::  (a -> Endo b) -> t a -> Endo b

foldr :: Foldable t => (a -> (b -> b)) -> b -> t a -> b
foldr f ini xs = appEndo (foldMap (Endo . f) xs) ini
-- foldr f ini xs = appEndo (foldMap (\x -> Endo (f x)) xs) ini

-------------------------------------------------------------------------------

pointful :: (t1 -> t2 -> t3) -> t1 -> (t4 -> t2) -> t4 -> t3
pointful = (.) (.)
--   \a b c d -> a b (c d)
--   \a b c d -> a b $ c d
--   \a b c d -> (a b) . c $ d
--   \a b c -> (a b) . c
--   \a b c -> (.) (a b) c
--   \a b c -> (.) (a b) $ c
--   \a b -> (.) (a b)
--   \a b -> (.) $ a b
--   \a b -> (.) . a $ b
--   \a -> (.) . a
--   \a -> (.) (.) $ a
--  (.) (.)

-------------------------------------------------------------------------------

data Ordering' = EQ' | GT' | LT'
  deriving (Show, Eq)

instance Semigroup Ordering' where
  (<>) :: Ordering' -> Ordering' -> Ordering'
  LT' <> _ = LT'
  EQ' <> x = x
  GT' <> _ = GT'

instance Monoid Ordering' where
  mempty :: Ordering'
  mempty = EQ'

-------------------------------------------------------------------------------

-- [] is monoid
testListMonoid :: [Integer]
testListMonoid = [1..10] `mappend` [11..20]

-- Maybe
testMaybeMonoid :: Maybe (Sum Int)
testMaybeMonoid = Just (Sum 23) <> Just (Sum 54)

-- First
-- Last
goFirst :: Maybe Integer
goFirst = getFirst $ First Nothing <> First (Just 23) <> First Nothing <> First (Just 50)

-- Sum
-- Product
goProduct :: Integer
goProduct = getProduct . mconcat $ fmap Product [1..4]

-- All
-- Any

-- Endo
hw :: Endo String
hw = Endo ("Hello, " ++) <> Endo (++ " and world!")

goHW :: String
goHW = appEndo hw "me"

-- Dual
dlast :: Maybe a -> Dual (First a)
dlast = Dual . First

firstToLast :: Maybe Int
firstToLast = getFirst . getDual $ dlast Nothing <> dlast (Just 23) <> dlast Nothing <> dlast (Just 50)

-------------------------------------------------------------------------------

data Pair a b = Pair a b
  deriving (Show)

instance Functor (Pair a) where
  fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair a b) = Pair a (f b)

instance Bifunctor Pair where
  bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
  bimap f g (Pair x y) = Pair (f x) (g y)

-- сравним Eq, Functor и Bifunctor -- они требуют разные типы типов

-- class Eq        (a :: Type)                 where
-- class Functor   (f :: Type -> Type)         where
-- class Bifunctor (p :: Type -> Type -> Type) where

-------------------------------------------------------------------------------
