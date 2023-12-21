{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Control.Monad.Trans.Class  (MonadTrans(..))
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char                  (toUpper)

newtype MaybeT m a = 
  MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . fmap Just

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT x) = MaybeT $ fmap (fmap f) x

instance (Monad m, Applicative m) => Applicative (MaybeT m) where
  pure :: (Monad m, Applicative m) => a -> MaybeT m a
  pure = lift . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) mf mx = do
    f <- mf
    x <- mx
    pure $ f x
  -- mf >>= \f ->
  --   mx >>= \x ->
  --     pure (f x)

instance Monad m => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) m k = MaybeT $ do
    x <- runMaybeT m
    case x of 
      Just v  -> runMaybeT (k v)
      Nothing -> pure Nothing

-------------------------------------------------------------------------------

secondElem :: Reader [String] String
secondElem = do
  el2 <- asks (map toUpper . head . tail)
  return el2

logFirst :: [String] -> Writer String String
logFirst xs = do
  let el1 = head xs
  let el2 = toUpper . head . tail <$> xs
  tell el1
  return el2

-- | with the package `transformers` you have to use `lift`:
--   import Control.Monad.Trans.Reader
--   import Control.Monad.Trans.Writer
--
logFirstAndRetSecond' :: WriterT String (Reader [String]) String
logFirstAndRetSecond' = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1
  return el2

-- | with the package `mtl` you don't have to use `lift`:
--   import Control.Monad.Reader
--   import Control.Monad.Writer
--
logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  tell el1
  return el2
-------------------------------------------------------------------------------

infixl 7 ***

-- (*) :: (Num a) => a -> a -> a

-- | `| a b -> c` to say that instance can be chosed by `a` and `b`
--   requires `{-# LANGUAGE FunctionalDependencies #-}`
class Mult a b c | a b -> c where
    (***) :: a -> b -> c

instance Mult Int Int Int where
    (***) = (*)

instance Mult Double Double Double where
    (***) :: Double -> Double -> Double
    (***) = (*)

instance Mult Int Double Double where
    i *** d = fromIntegral i * d

-- instance Mult Int Double Int where
--     (***) :: Int -> Double -> Int
--     i *** d = floor (fromIntegral i * d)

x :: Int
x = 2 :: Int

y :: Double
y = pi :: Double

-------------------------------------------------------------------------------