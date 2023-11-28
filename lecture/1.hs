-- Foldable
class Foldable t where
  {-# MINIMAL foldMap | foldr #-}
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b

-- foldMap через foldr
foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap toM xs = foldr (\x acc -> toM <> acc) mempty xs

-- foldr через foldMap
-- foldMap :: Monoid m => (a -> m) -> t a -> m

foldr :: (a -> b -> b) -> b -> t a -> b
foldr f ini xs = undefined