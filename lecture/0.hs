-- Магма / Полугруппа / Моноид
-- Foldable

-- Магама - множество с бинарной операцией на ней
-- S -> set
-- x :: S -> S -> S

-- Полугруппа - множество с бинарной операцией на ней
-- Обязана быть ассоциативной
-- S -> set
-- x :: S -> S -> S

-- class Semigroup a where
--   (<>) :: a -> a -> a

-- для полугруппы должны выполнять законы:
-- 1. x <> (y <> z) = (x <> y) <> z

-- NonEmpty a – непустой список гарантировано;
-- sconcat NonEmpty a -> a :-> reduce elements
-- stimes NonEmpty a -> a :-> repeats values n-times
-- :| -> for non-empty list

-- Monoid -> сужение полугруппы с нейтральным элементом
-- Triplet (S, x, 1)
-- S -> set
-- x :: S -> S -> S
-- Нейтральный элемент к операции

-- class (Semigroup m) => Monoid m where
--   mempty :: a -- 1
--   mappend :: a -> a -> a -- x
--   mappend = (<>)

-- 4 Laws
-- x <> mempty = x [Right identity]
-- mempty <> x  = x [Left identity]
-- x <> (y <> z) = (y <> z) <> x [Assoc]
-- mconcat = foldr (<>) mempty [Concatenation]
-- mconcat -> new method

-- List -> monoid
-- instance Semigroup [a] where
--   (<>) = (++)

-- instance Monoid [a] where
--   mempty = []

-- Maybe - monoid
-- instance Semigroup a => Semigroup (Maybe a) where
--   Nothing <> x = x
--   x <> Nothing = x
--   Just x <> Just y = Just (x <> y)

-- instance (Monoid a) => Monoid (Maybe a) where
--   mempty = Nothing

-- instance (Semigroup a) => Semigroup (Maybe a) where
--   Nothing <> x = x
--   x <> _ = x

-- instance (Monoid a) => Monoid (Maybe a) where
--   mempty = Nothing

-- Example for numbers
newtype Sum = Sum {getSum :: Int}

instance Semigroup Sum where
  Sum x <> Sum y = Sum (x + y)

instance Monoid Sum where
  mempty = Sum 0

-- Functions - monoids

newtype Endo a = Endo {getEndo :: a -> a}

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

-- Monoid is a monoid
newtype Dual m = Dual {getDual :: m}

instance (Semigroup m) => Semigroup (Dual m) where
  Dual m1 <> Dual m2 = Dual (m2 <> m1)

instance (Monoid m) => Monoid (Dual m) where
  mempty = Dual mempty

-- Associativity