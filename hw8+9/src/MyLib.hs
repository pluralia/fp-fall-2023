{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs, LambdaCase #-}

module MyLib where

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Control.Monad (guard)
import           Data.Monoid (Sum(..))

------------------------------------------------------------------------------

-- 6. Do-нотация (0,5 балла)
--    Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы

-- fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
-- fromDo11 aM bM = do
--     a <- fmap (+ 10) aM

--     -- в одном 'let'-выражении внутри do-нотации можно писать несколько связываний.
--     -- в обычных 'let'-выражениях это тоже работает
--     let aL = [a, a, a]
--         a  = a + length aL

--     return a

--     bM
--     [a, b, c] <- Just aL

--     b <- fmap (<> "abcd") bM

--     pure (c, b)

-- fromDo11' :: Maybe Int -> Maybe String -> Maybe (Int, String)
-- fromDo11' aM bM = fmap (+ 10) aM >>= \a -> 
--     let aL = [a, a, a]
--         a = a + length aL
--     in return a >> bM >> (\case
--         Just [a, b, c] -> fmap (<> "abcd") bM >>= \b -> pure (c, b)
--         _ -> undefined
--         ) (Just aL)

-- закомментирую, чтобы не ругался hlint. Если убрать a = a + length aL, то тест пройдёт.

-------------------------------------------------------------------------------

-- 7. Пифагоровы тройки (0,5 балла)

--    С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. Дублирования нужно убрать за счёт дополнительного условия в do-нотации
pythagor :: Int -> [(Int, Int, Int)]
pythagor n = take n $ do
    c <- [1..]
    b <- [1..c]
    a <- [1..b]
    guard (a*a + b*b == c*c)
    return (a, b, c)
-------------------------------------------------------------------------------

-- 9. Monad `Writer` (1,5 балла)

-- | Зададим свой Writer
newtype Writer' w a = Writer' { runWriter' :: (Identity a, w) }
  deriving (Show)

---------------------------------------

-- 9.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Writer' w) where
    fmap :: (a -> b) -> Writer' w a -> Writer' w b
    fmap f (Writer' (x, msg)) = Writer' (f <$> x, msg)

instance Monoid w => Applicative (Writer' w) where
    pure :: a -> Writer' w a
    pure x = Writer' (Identity x, mempty)

    (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
    Writer' (f, msg1) <*> Writer' (x, msg2) = Writer' (f <*> x, msg1 `mappend` msg2)

instance Monoid w => Monad (Writer' w) where
    (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
    Writer' (x, msg) >>= f = let (Identity a) = x
                              in let Writer' (y, msg') = f a
                                in Writer' (y, msg `mappend` msg')

---------------------------------------

-- 9.b Реализуйте инастанс класса `MonadWriter` для `Writer` (tell | writer, listen, pass) (0,5 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Strict.html#t:MonadWriter

instance (Monoid w) => MonadWriter w (Writer' w) where
    tell :: w -> Writer' w ()
    tell w = Writer' (Identity (), w)

    listen :: Writer' w a -> Writer' w (a, w)
    listen (Writer' (Identity x, msg)) = Writer' (Identity (x, msg), msg)

    pass :: Writer' w (a, w -> w) -> Writer' w a
    pass (Writer' (Identity (x, f), msg)) = Writer' (Identity x, f msg)

-- Почему нужно было определять `Writer' w a`, а не `Writer' a w`?
-- чтобы соответствовать структуре моноида и иметь фиксированный лог.

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

sumAndTraceInOrder :: Num a => BinaryTree a -> Writer' (Sum a) [a]
sumAndTraceInOrder Leaf = tell mempty >> pure []
sumAndTraceInOrder (Node v l r) = do
    leftResult <- sumAndTraceInOrder l
    tell (Sum v)
    (v :) <$> ((leftResult <>) <$> sumAndTraceInOrder r)

-------------------------------------------------------------------------------

-- 10. Monad `Reader` (1,75 балла)

-- | Зададим свой Reader
newtype Reader' r a = Reader' { runReader' :: r -> Identity a }

---------------------------------------

-- 10.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' w a -> Reader' w b
    fmap f r = Reader' (fmap f . runReader' r)

instance Applicative (Reader' r) where
    pure :: a -> Reader' r a
    pure a = Reader' $ \_ -> pure a

    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    rf <*> ra = Reader' $ \r -> runReader' rf r <*> runReader' ra r

instance Monad (Reader' r) where
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    ra >>= f = Reader' $ \r -> runReader' (f $ runIdentity $ runReader' ra r) r

---------------------------------------

-- 10.b Реализуйте инастанс класса `MonadReader` для `Reader` (ask | reader, local) (0,25 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#t:MonadReader

instance MonadReader r (Reader' r) where
    local :: (r -> r) -> Reader' r a -> Reader' r a
    local f rdr = Reader' $ \r -> runReader' rdr (f r)

    reader :: (r -> a) -> Reader' r a
    reader f = Reader' $ \r -> pure (f r)
---------------------------------------

-- 10.c Вычислите список утверждений с помощью Reader' (1 балл)

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
eval :: Expr -> Reader' Environment (Maybe Int)
eval (Primary (Var var)) = asks (M.lookup var)
eval (Primary (Val val)) = return $ Just val
eval (Binary leftExpr rightExpr) = do
    leftVal <- eval leftExpr
    rightVal <- eval rightExpr
    case (leftVal, rightVal) of
        (Just x, Just y) -> return $ Just (x + y)
        _ -> return Nothing

-- | Пример запуска вычисления выражения
--
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
evalStmts :: [Stmt] -> Reader' Environment (Maybe Int)
evalStmts [] = pure Nothing
evalStmts [stmt] = eval (expr stmt)
evalStmts (stmt:stmts) = do
  let v = eval (expr stmt)
  r <- ask
  case runIdentity $ runReader' v r of
    Just vv -> local (M.insert (name stmt) vv) (evalStmts stmts)
    Nothing -> evalStmts stmts

-- | Пример запуска вычисления списка утверждений
--
testEvalStmts :: Maybe Int
testEvalStmts = runIdentity $ runReader' (evalStmts [x, y, z, xx, w]) M.empty
  where
    x, y, z, xx, w :: Stmt
    x = Stmt "x" $ Primary . Val $ 2                                     -- x = 2
    y = Stmt "y" $ Primary . Val $ 3                                     -- y = 3
    z = Stmt "z" $ Binary (Primary . Var $ "x") (Primary . Var $ "y")    -- z = 5
    xx = Stmt "x" $ Binary (Primary . Var $ "x") (Primary . Var $ "x")   -- xx = 4
    w = Stmt "w" $ Binary (Primary . Var $ "z") (Primary . Var $ "x")    -- w = 9

-------------------------------------------------------------------------------