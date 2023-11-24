module Prac where
imporst Data.Functor.Identity

------------------------------------------------------------------------------

-- Опасность return: ведет себя не так же как в императивных языках

wrapnsucc :: Int -> Identity Int
wrapnsucc = Identity . succ

succ3 :: Identity Int
succ3 = do 
  let i = 3
  x <- wrapnsucc i
  y <- wrapnsucc x
  z <- wrapnsucc y
  return z

succ3' :: Identity Int
succ3' = do
  let i = 3
  x <- wrapnsucc i
  y <- wrapnsucc x
  return y             -- вычисление не остановится
  z <- wrapnsucc y
  return z             -- вернет 6

-- можно использовать pure, чтобы не перепутать
succ3pure :: Identity Int
succ3pure = do 
  let i = 3
  x <- wrapnsucc i
  y <- wrapnsucc x
  pure y
  z <- wrapnsucc y
  pure z

------------------------------------------------------------------------------

-- Отличие Monad и Applicative

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM condM thenM elseM = do
    cond <- condM
    if cond 
      then thenM
      else elseM

ifThenElseA :: Applicative f => f Bool -> f a -> f a -> f a
ifThenElseA condA thenA elseA = ifThenElse <$> condA <*> thenA <*> elseA
  where
    ifThenElse :: Bool -> a -> a -> a
    ifThenElse cond th el = if cond then th else el

myFmap :: Monad m => (a -> b) -> m a -> m b
myFmap f ma = do
    x <- ma
    pure $ f x

myApp :: Monad m => m (a -> b) -> m a -> m b
myApp fM ma = do
    f <- fM
    a <- ma
    pure $ f a

------------------------------------------------------------------------------

-- Дешугаринг

term :: MonadFail m => m [m b] -> m [a] -> m b
term a b = do
    a
    b 
    if 3 < 5
        then do
            a
            pure 4 
        else pure 5
    x <- a 
    let y = 5
    [x, y] <- b
    [x, y, z] <- a
    x' <- x
    pure x'

term' :: MonadFail m => m [m b] -> m [a] -> m b
term' a b = a
    >> b 
    >> (if 3 < 5 then a >> pure 4 else pure 5) 
    >> a >>= (\x -> 
        let y = 5 in
        b >>= (\b' ->
          case b' of
            [x, y] -> a >>= (\a' -> 
                              case a' of
                                [x, y, z] -> x >>= (\x' ->
                                                    pure x'
                                                    )
                                _         -> fail ""
                            )
            _      -> fail ""
               )
    )
