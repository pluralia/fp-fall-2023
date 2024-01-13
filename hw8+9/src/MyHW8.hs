{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyHW8 where
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Functor.Identity
import qualified Data.Map.Strict           as M
import           Prelude

------------------------------------------------------------------------------

-- 6. Do-нотация (0,5 балла)
--    Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы

fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
fromDo11 aM bM = do
    a <- fmap (+ 10) aM

    -- в одном 'let'-выражении внутри do-нотации можно писать несколько связываний.
    -- в обычных 'let'-выражениях это тоже работает
    let aL = [a, a, a]
        a  = a + length aL

    return a

    bM
    [a, b, c] <- Just aL

    b <- fmap (<> "abcd") bM

    pure (c, b)

-- fmap :: (a -> b) -> t a -> t b
-- >>= :: m a -> (a -> m b) -> m b

-- :p понял, что происходит внутри, но сам написать не смог
withoutDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
withoutDo11 aM bM = fmap (+10) aM
    >>= \a ->
    let aL = [a, a, a]
        a = a + length aL
    in
        return a >>
        bM >>
        Just aL >>=
          \lst ->
                case lst of
                    [a, b, c] -> fmap (<> "abcd") bM >>= (\b ->
                        pure (c, b))
                    _ -> fail ""
                    
    -- в одном 'let'-выражении внутри do-нотации можно писать несколько связываний.
    -- в обычных 'let'-выражениях это тоже работает

-------------------------------------------------------------------------------

-- 7. Пифагоровы тройки (0,5 балла)

--    С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. Дублирования нужно убрать за счёт дополнительного условия в do-нотации

pythagoreanTriple :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriple x = do
    c <- [1 .. x]
    a <- [1 .. c] 
    b <- [1 .. c] 
    True <- return $ (a^2 + b^2 == c^2) && (a < b)
    return (a, b, c)

-------------------------------------------------------------------------------

-- 9. Monad `Writer` (1,5 балла)

-- | Зададим свой Writer
-- newtype Writer' w a = Writer' { runWriter' :: (Identity a, w) }
newtype Writer' w a = Writer' { runWriter' ::  (,) (Identity a) w }
  deriving (Show, Eq)

---------------------------------------

-- 9.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Writer' w) where
    fmap :: (a -> b) -> Writer' w a -> Writer' w b
    fmap f (Writer' (Identity a, w)) = Writer'(Identity $ f a,  w)

instance Monoid w => Applicative (Writer' w) where
    pure :: a -> Writer' w a
    pure x = Writer' (Identity x, mempty)

    (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
    (<*>) (Writer' (Identity f, w)) (Writer' (Identity x, w1)) = Writer' (Identity $ f x, w <> w1)

instance Monoid w => Monad (Writer' w) where
    (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
    (>>=) (Writer' (Identity x, w)) f = Writer' (Identity y, w <> w1)
      where
        (Writer' (Identity y, w1)) = f x

-- instance Monoid w => Monad (Writer' w) where
--     (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
--     (>>=) m f = Writer' (Identity b, w <> w')
--         where
--             (Identity a, w) = runWriter' m
--             (Identity b, w') = runWriter' $ f a
---------------------------------------

-- 9.b Реализуйте инастанс класса `MonadWriter` для `Writer` (tell | writer, listen, pass) (0,5 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Strict.html#t:MonadWriter

-- не особо понял, что делают эти функции, просто заполнил, чтобы по типам подходило :(
instance (Monoid w) => MonadWriter w (Writer' w) where
    tell :: w -> Writer' w ()
    tell w = Writer' (Identity (), w)

    listen :: Writer' w a -> Writer' w (a, w)
    listen (Writer' (Identity a, w)) = Writer' (Identity (a, w), w)

    pass :: Writer' w (a, w -> w) -> Writer' w a
    pass m = Writer' (Identity a, f w)
      where (Identity (a, f), w) = runWriter' m
      

-- Почему нужно было определять `Writer' w a`, а не `Writer' a w`?
-- монады являются однопараметрическими, поэтому нужно фиксировать один параметр (Monoid w) 
---------------------------------------

-- 9.c Реализуйте обход (любой) бинарного дерева и суммируйте элементы в вершинах с помощью Writer'
--     (постарайтесь использовать функции из MonadWriter) (0,5 балла)

data BinaryTree a
  = Leaf
  | Node
    { nodeValue :: a
    , leftChild  :: BinaryTree a
    , rightChild :: BinaryTree a
    }
  deriving (Show, Eq)

-- :p понятно
sumAndTraceInOrder :: Num a => BinaryTree a -> Writer' (Sum a) [a]
sumAndTraceInOrder Leaf = return []
sumAndTraceInOrder (Node val l r) = do
    tell $ Sum val
    resLeft <- sumAndTraceInOrder l
    resRight <- sumAndTraceInOrder r
    return $ val : (resLeft ++ resRight)

  
  -- Sum a -> Writer' (Sum a) ()
  
  -- Writer' w a
  -- Writer {runWriter :: (a, w)}
  
  -- (>>=) :: m a -> (a -> m b) -> m b 
  -- (>>) :: m a -> m b -> m b 

  -- tell :: w -> Writer' w ()
  -- tell w = Writer' (Identity (), w)
-------------------------------------------------------------------------------

-- 10. Monad `Reader` (1,75 балла)

-- | Зададим свой Reader
-- newtype Reader' r a = Reader' { runReader' :: (->) r (Identity a) }

---------------------------------------

-- 10.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

newtype Reader' r a = Reader' { runReader' :: r -> Identity a }
instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' r a -> Reader' r b
    fmap f (Reader' g) = Reader' $ \e ->
      let
        Identity x0 = g e
        x1 = f x0
      in
        Identity x1


instance Applicative (Reader' r) where
    pure :: a -> Reader' r a
    pure x = Reader' (\_ -> Identity x)

-- newtype Reader' r a = Reader' { runReader' :: r -> Identity a }
-- Reader' r -> Identity (a -> b)
-- Reader' r -> Identity a
    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    (<*>) mf mr = Reader' $ \e ->
      let
        Identity f = runReader' mf e
        Identity x = runReader' mr e
      in
        Identity $ f x


instance Monad (Reader' r) where
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    (>>=) mr k = Reader' $ \e -> 
      let 
        Identity x = runReader' mr e
      in
       (runReader' $ k x) e

---------------------------------------

-- 10.b Реализуйте инастанс класса `MonadReader` для `Reader` (ask | reader, local) (0,25 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#t:MonadReader


instance MonadReader r (Reader' r) where
-- newtype Reader' r a = Reader' { runReader' :: r -> Identity r }
    ask :: Reader' r r
    ask = Reader' $ \e -> Identity e

-- newtype Reader' r a = Reader' { runReader' :: r -> Identity a }
    local :: (r -> r) -> Reader' r a -> Reader' r a
    local f mr = Reader' $ \e ->
      let
        g = runReader' mr
      in
        g . f $ e


---------------------------------------

-- 9.c Вычислите список утверждений с помощью Reader' (1 балл)

-- | Выражение будет задавать в виде бинарного дерева
--
data Expr
  = Primary { item :: Item }  -- значение в листьях
  | Binary                    -- операция в узлах; договоримся, что у нас возможно только сложение, поэтому операцию никак не специфицируем
    { left  :: Expr
    , right :: Expr
    }
  deriving (Show, Eq)

-- | Элементы могут быть числами или переменными; числа -- готовый результат, а переменные нужно найти в окружении
--
data Item = Var String | Val Int
  deriving (Show, Eq)

-- | Окружение зададим Map
--
type Environment = M.Map String Int

-- | Вычислите выражение, используя Reader'
--   Если выражение использует необъявленную переменную, вернем Nothing
--
-- списал с лекции, ничего не понял :(
eval :: Expr -> Reader' Environment (Maybe Int)
eval (Primary (Val x)) = return . Just $ x
eval (Primary (Var x)) = reader $ \r -> M.lookup x r
eval (Binary left' right') =
    eval left' >>= (\resLeft ->
        eval right' >>= (\resRight ->
            pure $ resLeft >>= (\x -> resRight >>= (\y -> pure $ x + y))
        )
    )

-- | Пример запуска вычисления выражения
--
-- списал с лекции, ничего не понял :(
testEvalExpr :: Maybe Int -- ожидаем `Just 5`
testEvalExpr = runIdentity $ runReader' (eval expr') env
  where
    env :: Environment
    env = M.fromList [("x", 3)]

    expr' :: Expr
    expr' = Binary (Primary . Val $ 2) (Primary . Var $ "x")

-- | Утверждение будем задавать как декларацию переменной
--
data Stmt = Stmt
    { name :: String
    , expr :: Expr
    } deriving (Show, Eq)

-- | Вычислите список утверждений, используя Reader' (постарайтесь использовать функции из MonadReader)
--   В качестве результата вычисления всего списка договоримся возвращать результат вычисления последнего выражения в списке
--
evalStmts' :: [Stmt] -> Reader' Environment (Maybe Int)
evalStmts' []                      = return Nothing
evalStmts' [Stmt _ expr']           = eval expr'
evalStmts' ((Stmt name' expr') : xs) =
    eval expr' >>= (\maybeVal -> ask >>= (\_ ->local (maybe id (M.insert name') maybeVal) (evalStmts' xs)))


-- | Пример запуска вычисления списка утверждений
--
testEvalStmts :: Maybe Int
testEvalStmts = runIdentity $ runReader' (evalStmts' [x, y, z, xx, w]) M.empty
  where
    x, y, z, xx, w :: Stmt
    x = Stmt "x" $ Primary . Val $ 2                                     -- x = 2
    y = Stmt "y" $ Primary . Val $ 3                                     -- y = 3
    z = Stmt "z" $ Binary (Primary . Var $ "x") (Primary . Var $ "y")    -- z = 5
    xx = Stmt "x" $ Binary (Primary . Var $ "x") (Primary . Var $ "x")   -- xx = 4
    w = Stmt "w" $ Binary (Primary . Var $ "z") (Primary . Var $ "x")    -- w = 9

-------------------------------------------------------------------------------