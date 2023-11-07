import qualified Data.Text as T

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f fa fb = f <$> fa <*> fb

-- fmap :: (x -> y) -> f x -> f y
-- f :: a -> b -> c
-- x -> y ~ a -> b -> c

-- x = a, y = b -> c
-- fmap :: (a -> (b -> c)) -> f a -> f (b -> c)
-- f :: a -> (b -> c)
-- fmap f :: f a -> f (b -> c)

-- fa :: f a
-- ffa = fmap f fa :: f (b -> c)

-- ffa :: f (b -> c)
-- fb :: f b
-- res :: f c
-- (<*>) ::  f (b -> c) -> f b -> f c

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f fa fb fc = f <$> fa <*> fb <*> fc

-- fmap :: (x -> y) -> f x -> f y
-- f :: (a -> b -> c -> d) = (a -> (b -> c -> d))
-- x = a, y = b -> c -> d
-- fmap :: (a -> (b -> c -> d)) -> f a -> f (b -> c -> d)
-- fmap f :: f a -> f (b -> c -> d)
-- ffa = f <$> fa :: f (b -> c -> d)

-- ffa :: f (b -> c -> d)
-- fb :: f b
-- fc :: f c
-- res :: f d

-- (<*>) ::  f (x -> y) -> f x -> f y
-- x = b, y = c -> d
-- (<*>) ::  f (b -> (c -> d)) -> f b -> f (c -> d)
-- ffa <*> :: f b -> f (c -> d)
-- ffafb = ffa <*> fb :: f (c -> d)

-- ffafb :: f (c -> d)
-- fc :: f c
-- res :: f d
-- (<*>) ::  f (x -> y) -> f x -> f y
-- x = c, y = d
