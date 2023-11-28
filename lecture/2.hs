data Ordering' = Eq' | GT' | Left'

instance Semigroup Ordering' where
  (<>) :: Ordering' -> Ordering' -> Ordering'

  LT' <> = LT'
  EQ' <> x = x
  GT' <> _ = GT'

-- x <> EQ = x
-- EQ <> x = x
-- x <> (y <> z) = (x <> y) <> z
-- LT' = 

-- check laws in hw
-- fmap -> functor

-- gitFirst . getDual $ last Nothing <> last (Just 23) <> last Nothing <> last (Just 50)

-- Foldable
-- Maybe a
-- a is in context of Maybe

-- pure оборачивает в контекст