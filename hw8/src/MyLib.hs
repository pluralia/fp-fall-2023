{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}
module MyLib where

{- cabal:
    build-depends: base, mtl, containers
-}
{- install for ghci:
    > cabal install mtl
    > ghci
    >> :set -package mtl
-}
import           Control.Monad.Writer.Strict
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Monoid (Sum(..))
import           Data.List (nub)
import Data.Maybe

-------------------------------------------------------------------------------

-- 1. Travserable (1,5 балла)

---------------------------------------

-- 1.a Реализуйте инстансы Traversable для Maybe и списка (без док-ва законов) (0,5 балла)

---------------------------------------

-- 1.b Реализуйте `traverse` через `sequenceA` и `sequenceA` через `traverse` (0,5 балла)

traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' = undefined

sequenceA' :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA' = undefined

---------------------------------------

-- 1.c В чем разница между Traversable и Functor? Между Traversable и Foldable? (0,5 балла)

-------------------------------------------------------------------------------

-- | 2. Реализуйте `rejectWithNegatives`, которая возвращает исходный список, обернутый в Just,
--       если в нем нет отрицательных элементов, и Nothing в противном случае (0,5 балла)
--
rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = undefined
  where
    deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
    deleteIfNegative x = if x < 0 then Nothing else Just x

-------------------------------------------------------------------------------

-- | 3. Рассмотрим представление матриц в виде вложенных списков, в которых внутренние списки являются строками.
--       Используйте Traversable для реализации транспонирования матриц (0,5 балла)
--
transpose :: [[a]] -> [[a]]
transpose = undefined

-------------------------------------------------------------------------------

-- 4. Для чего нужен класс типов MonadFail? (0,25 балла)

-------------------------------------------------------------------------------

-- | 5. Сделайте (WithData d) монадой и не забудьте про 'MonadFail'.
--       Опишите словами, какой эффект получился у созданной вами монады.
--       Без описания задание не засчитывается (0,5 балла)
--
newtype WithData d a = WithData { runWithData :: d -> a }

-------------------------------------------------------------------------------

-- 6. Do-нотация (1 балл)

---------------------------------------

-- | 6.a Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы (0,5 балла)
--
fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
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

fromDo11 aM bM =
    aM Control.Monad.Reader.>>= f . (+ 10)
    where
        f a1 = let aL = [a1, a1, a1]
                   a  = a1 + length aL
               in 
                  bM >>
                  g aL a
        g aL _ = case aL of
            [_, _, c] -> bM Control.Monad.Reader.>>= h c . (<> "abcd")
            _ -> Nothing 
        h c b = pure (c, b)
---------------------------------------

-- | 6.b Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы (0,5 балла)
--
fromDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
-- fromDo12 isL cM = do
--     curI  <- isL
--     nextI <- tail isL

--     -- в do-нотации можно использовать конструкцию if-then-else.
--     -- исполнение кода пойдёт по одной из веток в зависимости от условия
--     if nextI > curI
--         then do
--             let a = curI + nextI

--             nextNextI <- tail $ tail isL
--             Just ch   <- [cM]

--             -- в do-нотации можно использовать паттерн-матчинг.
--             -- аналогично if-then-else код исполняется по одной из веток
--             -- в зависимости от того, какая ветка сматчилась
--             case (curI, nextI, nextNextI) of
--               (0, 0, 0) -> pure (ch, a)
--               _         -> fail ""
--         else pure ('0', 0)
fromDo12 isL cM = 
    isL Control.Monad.Reader.>>= \curI ->
    tail isL Control.Monad.Reader.>>= \nextI ->
    if nextI > curI
        then let a = curI + nextI in
            tail (tail isL) Control.Monad.Reader.>>= \nextNextI ->
            case cM of
                Nothing -> fail ""
                Just ch -> case (curI, nextI, nextNextI) of
                    (0, 0, 0) -> pure (ch, a)
                    _         -> fail ""
        else pure ('0', 0)
-------------------------------------------------------------------------------

-- 7. С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. Дублирования нужно убрать за счёт дополнительного условия в do-нотации (0,5 балла)
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = nub $ do
  a <- [1..n]
  b <- [1..n]
  c <- [1..n]
  if a*a + b*b == c*c
    then return (a, b, c)
    else []
-------------------------------------------------------------------------------

-- 8. Задайте тип данных (ReturnableCalculation a) и сделайте 'ReturnableCalculation' монадой.
--    Также реализуйте функцию 'realReturn' (2 балла)

--    Монада для 'ReturnableCalculation' должна быть определена таким образом,
--    чтобы 'realReturn', написанный в do-нотации или в цепочке bind'ов,
--    действительно возвращал заданное значение, а последующие вычисления
--    не имели бы никакого значения.

-- | Пример использования 'realReturn'.
--   Должно вернуться 42, завёрнутое в 'ReturnableCalculation'.
--
-- returnExample :: ReturnableCalculation Int
-- returnExample = do
--     let a = 40
--         b = 2

--     realReturn $ a + b

--     let a = 0

--     if a == 0
--       then pure 200
--       else realReturn 0

newtype ReturnableCalculation a = ReturnableCalculation (Maybe a, Bool)
  deriving (Show, Eq)

instance Functor ReturnableCalculation where
  fmap :: (a -> b) -> ReturnableCalculation a -> ReturnableCalculation b
  fmap f (ReturnableCalculation (Just x, b)) = ReturnableCalculation (Just (f x), b)
  fmap _ (ReturnableCalculation (Nothing, b)) = ReturnableCalculation (Nothing, b)

instance Applicative ReturnableCalculation where
  pure :: a -> ReturnableCalculation a
  pure x = ReturnableCalculation (Just x, True)

  (<*>) :: ReturnableCalculation (a -> b)-> ReturnableCalculation a -> ReturnableCalculation b
  ReturnableCalculation (Just f, True) <*> ReturnableCalculation (Just x, True) = ReturnableCalculation (Just (f x), True)
  _ <*> _ = ReturnableCalculation (Nothing, False)

-- instance Monad ReturnableCalculation where
--   (>>=) :: ReturnableCalculation a-> (a -> ReturnableCalculation b) -> ReturnableCalculation b
--   ReturnableCalculation (Just x, True) >>= f = f x
--   ReturnableCalculation (Just x, False) >>= _ = pure x
--   _ >>= _ = ReturnableCalculation (Nothing, False)

realReturn :: a -> ReturnableCalculation a
realReturn x = ReturnableCalculation (Just x, False)



-------------------------------------------------------------------------------

-- 9. Monad `Writer` (1,5 балла)

-- | Зададим свой Writer
newtype Writer' w a = Writer' { runWriter' :: (Identity a, w) }
  deriving (Show)

---------------------------------------

-- 9.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Writer' w) where
    fmap :: (a -> b) -> Writer' w a -> Writer' w b
    fmap f (Writer' (Identity a, w)) = Writer' (Identity (f a), w)

instance Monoid w => Applicative (Writer' w) where
    pure :: a -> Writer' w a
    pure a = Writer' (Identity a, mempty)

    (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
    Writer' (Identity f, w1) <*> Writer' (Identity a, w2) = Writer' (Identity (f a), mappend w1 w2)

instance Monoid w => Monad (Writer' w) where
    (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
    Writer' (Identity a, w1) >>= f = let Writer' (Identity b, w2) = f a in Writer' (Identity b, mappend w1 w2)

---------------------------------------

-- 9.b Реализуйте инастанс класса `MonadWriter` для `Writer` (tell | writer, listen, pass) (0,5 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Strict.html#t:MonadWriter

instance (Monoid w) => MonadWriter w (Writer' w) where
    tell :: w -> Writer' w ()
    tell w = Writer' (Identity (), w)

    listen :: Writer' w a -> Writer' w (a, w)
    listen m = Writer' (Identity (runIdentity a, w), w)
        where
            (a, w) = runWriter' m

    pass :: Writer' w (a, w -> w) -> Writer' w a
    pass m = Writer' (Identity a, f w)
        where
            (Identity (a, f), w) = runWriter' m

-- Почему нужно было определять `Writer' w a`, а не `Writer' a w`?
-- Ощущение, что просто логичнее и удобнее если трезультат идет первее
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
sumAndTraceInOrder Leaf = return []
sumAndTraceInOrder (Node value left' right') = leftSum >>= rightSum
  where
    leftSum = sumAndTraceInOrder left'
    rightSum leftResult = tell (Sum value) >> sumAndTraceInOrder right' >>= result leftResult
    result leftResult rightResult = return (leftResult ++ [value] ++ rightResult)

-------------------------------------------------------------------------------

-- 10. Monad `Reader` (1,75 балла)

-- | Зададим свой Reader
newtype Reader' r a = Reader' { runReader' :: r -> Identity a }

---------------------------------------

-- 10.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' w a -> Reader' w b
    fmap f (Reader' g) = Reader' h
        where
            h r = Identity $ f (runIdentity (g r))

instance Applicative (Reader' r) where
    pure :: a -> Reader' r a
    pure a = Reader' h
        where
            h _ = Identity a

    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    (Reader' f) <*> (Reader' a) = Reader' h
        where
            h r = Identity $ runIdentity (f r) (runIdentity (a r))

instance Monad (Reader' r) where
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    (Reader' a) >>= f = Reader' h
        where
            h r = runReader' (f (runIdentity (a r))) r

---------------------------------------

-- 10.b Реализуйте инастанс класса `MonadReader` для `Reader` (ask | reader, local) (0,25 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#t:MonadReader

instance MonadReader r (Reader' r) where
    ask :: Reader' r r
    ask = reader'
        where
            reader' = Reader' $ \r -> Identity r

    local :: (r -> r) -> Reader' r a -> Reader' r a
    local f reader' = Reader' h
        where
            h r = runReader' reader' (f r)

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
eval :: Expr -> Reader' Environment (Maybe Int)
eval (Primary (Var x)) = M.lookup x <$> ask  -- Ищем значение переменной в окружении
eval (Primary (Val n)) = return $ Just n  -- Если выражение - число, возвращаем его
eval (Binary l r) = do
    l' <- eval l  --  вычисляем левое подвыражение
    r' <- eval r  --  вычисляем правое подвыражение
    return $ (+) <$> l' <*> r'  -- складываем

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
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
evalStmts :: [Stmt] -> Reader' Environment (Maybe Int)
evalStmts = go Nothing
  where
    go :: Maybe Int -> [Stmt] -> Reader' Environment (Maybe Int)
    go lastResult [] = return lastResult
    go _ (stmt:stmts) = do
        result <- eval (expr stmt)
        let n = fromMaybe 0 result  -- Используем 0, если вычисление не удалось
        local (M.insert (name stmt) n) (go result stmts)  -- Обновляем окружение независимо от результата вычисления

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


