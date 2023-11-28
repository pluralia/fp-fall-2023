{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module MyLib where

import           Control.Monad.Writer.Strict
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Monoid (Sum(..))
import           Control.Applicative (ZipList(..))

-------------------------------------------------------------------------------

-- 1. Traversable (1,5 балла)

---------------------------------------

-- 1.a Реализуйте инстансы Traversable для Maybe и списка (без док-ва законов) (0,5 балла)

data Maybe' a = Nothing' | Just' a
  deriving (Show, Eq)

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing'  = Nothing'
  fmap f (Just' x) = Just' $ f x

instance Foldable Maybe' where
  foldr :: (a -> b -> b) -> b -> Maybe' a -> b
  foldr _ ini Nothing'  = ini
  foldr f ini (Just' x) = f x ini

instance Traversable Maybe' where
  traverse :: (Applicative f) => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse _ Nothing'  = pure Nothing'
  traverse f (Just' x) = Just' <$> f x


data List a = Null | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Null         = Null
  fmap f (Cons xx xs) = Cons (f xx) (fmap f xs)

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ ini  Null       = ini
  foldr f ini (Cons x xs) = f x (foldr f ini xs)

instance Traversable List where
  traverse :: (Applicative f) => (a -> f b) -> List a -> f (List b)
  traverse _  Null       = pure Null
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

---------------------------------------

-- 1.b Реализуйте `traverse` через `sequenceA` и `sequenceA` через `traverse` (0,5 балла)

traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA' . fmap f

sequenceA' :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA' = traverse' id

---------------------------------------

-- 1.c В чем разница между Traversable и Functor? Между Traversable и Foldable? (0,5 балла)

-- между Traversable и Functor: 
--    Functor вытаскивает тип из какого-то контейнера и применяет к нему функцию, изменяя тип,
--    но сохраняя его в том же контейнере, а Traversable может работать с какими-то "побочными" 
--    эффектами, меняя внешний и внутренний контейнер местами: например [Maybe a] -> Maybe [a]

-- между Traversable и Foldable:
--    Foldable осуществляет свёртку на типах, завёрнутыми в контейнер - то есть он их может 
--    как-то преобразовать и выдать результат на всех элементах в контейнере, который не завёрнут
--    в этот контейнер. Traversable в свою очередь не считает результат на всех элементах, он
--    изменяет каждый элемент, меняя контейнеры местами.

-------------------------------------------------------------------------------

-- | 2. Реализуйте `rejectWithNegatives`, которая возвращает исходный список, обернутый в Just,
--       если в нем нет отрицательных элементов, и Nothing в противном случае (0,5 балла)
--
rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = traverse deleteIfNegative
  where
    deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
    deleteIfNegative x = if x < 0 then Nothing else Just x

-------------------------------------------------------------------------------

-- | 3. Рассмотрим представление матриц в виде вложенных списков, в которых внутренние списки являются строками.
--       Используйте Traversable для реализации транспонирования матриц (0,5 балла)
--
transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

-------------------------------------------------------------------------------

-- 4. Для чего нужен класс типов MonadFail? (0,25 балла)

-- для обработки ситуаций в case ... of, когда нам приходит какое-то значение, 
-- которое не удовлетворяет нашему паттерн-матчингу
-- или для возвращения ошибки в конструкции it - then - else

-------------------------------------------------------------------------------

-- | 5. Сделайте (WithData d) монадой и не забудьте про 'MonadFail'.
--       Опишите словами, какой эффект получился у созданной вами монады.
--       Без описания задание не засчитывается (0,5 балла)
--
newtype WithData d a = WithData { runWithData :: d -> a }

instance Functor (WithData d) where
  fmap :: (a -> b) -> WithData d a -> WithData d b
  fmap f aR = WithData bR
    where
      bR w = f $ runWithData aR w

instance Applicative (WithData d) where
    pure :: a -> WithData d a
    pure = WithData . const

    (<*>) :: WithData d (a -> b) -> WithData d a -> WithData d b
    (<*>) fR aR = WithData $ \w -> bR w
      where
        f    = runWithData fR
        bR w = f w $ runWithData aR w

instance Monad (WithData d) where
    (>>=) :: WithData d a -> (a -> WithData d b) -> WithData d b
    (>>=) aR k = WithData $ \w -> runWithData (k $ runWithData aR w) w

instance MonadFail (WithData d) where
  fail :: String -> WithData d a
  fail = error

-- WithData похожа на Reader - то есть она принимает какое-то окружение и возвращает значение,
-- посчитанное на этом окружении. 

-- При реализации инстанса MonadFail у меня возникла проблема с типом `a` : непонятно, что будет выдаваться 
-- при ошибке. Идеальным был бы случай, если бы для типа `a` были установлены ограничения на классы типов - 
-- `(Monoid a)` или `(MonadFail a)`, потому что тогда был бы определён `mempty a` или `fail a`.
-- Но так как таких ограничений нет, я просто вернула ошибку.

-------------------------------------------------------------------------------

-- + 6. Do-нотация (1 балл)

---------------------------------------

-- | 6.a Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы (0,5 балла)
--
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

withoutDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
withoutDo11 aM bM = fmap (+ 10) aM >>= (\a ->
                    let aL = [a, a, a]; a = a + length aL in
                    return a >> (bM
                      >> (Just aL
                          >>= (\aL' ->
                            case aL' of
                              [a, b, c] ->
                                fmap (<> "abcd") bM >>= (\b ->
                                  pure (c, b)
                                )
                              _ -> fail ""
                          )
                      )
                    )
                  )

---------------------------------------

-- | 6.b Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы (0,5 балла)
--
fromDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
fromDo12 isL cM = do
    curI  <- isL
    nextI <- tail isL

    -- в do-нотации можно использовать конструкцию if-then-else.
    -- исполнение кода пойдёт по одной из веток в зависимости от условия
    if nextI > curI
        then do
            let a = curI + nextI

            nextNextI <- tail $ tail isL
            Just ch   <- [cM]

            -- в do-нотации можно использовать паттерн-матчинг.
            -- аналогично if-then-else код исполняется по одной из веток
            -- в зависимости от того, какая ветка сматчилась
            case (curI, nextI, nextNextI) of
              (0, 0, 0) -> pure (ch, a)
              _         -> fail ""
        else pure ('0', 0)

withoutDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
withoutDo12 isL cM =
  isL >>= (\curI ->
    tail isL >>= (\nextI ->
      (if nextI > curI
        then  let a = curI + nextI in
              (tail $ tail isL) >>= (\nextNextI ->
                [cM] >>= (\x ->
                  case x of
                    (Just ch) ->
                      (case (curI, nextI, nextNextI) of
                        (0, 0, 0) -> pure (ch, a)
                        _         -> fail "" )
                    _         -> fail ""
                        )
                      )
        else pure ('0', 0)
        )
      )
    )

-------------------------------------------------------------------------------

-- + 7. С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. Дублирования нужно убрать за счёт дополнительного условия в do-нотации (0,5 балла)

pifagor :: Int -> [(Int, Int, Int)]
pifagor n = do
  a <- [1..n]  -- фиксируем a
  b <- [1..a]  -- не бесконечное число вариантов
  c <- [1..b]
  if a*a == b*b + c*c
    then return (a, b, c)
    else fail ""

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
returnExample :: ReturnableCalculation Int
returnExample = do
    let a = 40
        b = 2

    realReturn $ a + b

    let a = 0

    if a == 0
      then pure 200
      else realReturn 0

newtype ReturnableCalculation' a b = ReturnableCalculation' { runRC :: Either a b }
  deriving (Show, Eq)

type ReturnableCalculation a = ReturnableCalculation' a a

-- Left - realReturn был 

instance Functor (ReturnableCalculation' a) where
    fmap :: (b -> c) -> ReturnableCalculation' a b -> ReturnableCalculation' a c
    fmap f rc  = f <$> rc
    -- fmap f (ReturnableCalculation' e) = ReturnableCalculation' (fmap f e)

instance Applicative (ReturnableCalculation' a) where
    pure :: b -> ReturnableCalculation' a b 
    pure = ReturnableCalculation' . Right

    (<*>) :: ReturnableCalculation' a (b -> c) -> ReturnableCalculation' a b -> ReturnableCalculation' a c
    (<*>) (ReturnableCalculation' f) (ReturnableCalculation' e) = ReturnableCalculation' (f <*> e)

instance Monad (ReturnableCalculation' a) where
    (>>=) :: ReturnableCalculation' a b -> (b -> ReturnableCalculation' a c) -> ReturnableCalculation' a c
    (>>=) (ReturnableCalculation' (Left x)) _ = ReturnableCalculation' (Left x)
    (>>=) (ReturnableCalculation' (Right x)) k = k x

realReturn :: a -> ReturnableCalculation' a b
realReturn = ReturnableCalculation' . Left

-------------------------------------------------------------------------------

-- + 9. Monad `Writer` (1,5 балла)

-- | Зададим свой Writer
newtype Writer' w a = Writer' { runWriter' :: (Identity a, w) }
  deriving (Show, Eq)

---------------------------------------

-- 9.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Writer' w) where
    fmap :: (a -> b) -> Writer' w a -> Writer' w b
    fmap f aW = Writer' $ (\ (Identity x, logger) -> (Identity (f x), logger)) (runWriter' aW)

instance Monoid w => Applicative (Writer' w) where
    pure :: a -> Writer' w a
    pure x = Writer' (Identity x, mempty)

    (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
    (<*>) (Writer' (Identity f, logger1)) (Writer' (Identity x, logger2)) =
      Writer' (Identity (f x), logger1 <> logger2)

instance Monoid w => Monad (Writer' w) where
    (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
    (>>=) (Writer' (Identity x, logger1)) k = Writer' (Identity new_x, logger1 <> logger2)
      where
        Writer' (Identity new_x, logger2) = k x

---------------------------------------

-- 9.b Реализуйте инастанс класса `MonadWriter` для `Writer` (tell | writer, listen, pass) (0,5 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Strict.html#t:MonadWriter

instance (Monoid w) => MonadWriter w (Writer' w) where
    tell :: w -> Writer' w ()
    tell logger = Writer' (Identity (), logger)

    listen :: Writer' w a -> Writer' w (a, w)
    listen (Writer' (Identity x, logger)) = Writer' (Identity (x, logger), logger)

    pass :: Writer' w (a, w -> w) -> Writer' w a
    pass (Writer' (Identity (x, f), logger)) = Writer' (Identity x, f logger)

-- Почему нужно было определять `Writer' w a`, а не `Writer' a w`?

-- Потому что у нас logger всегда одного типа (моноид) и мы его фиксируем

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
sumAndTraceInOrder Leaf = pure []
sumAndTraceInOrder (Node val leftCh rightCh) = do
      tell (Sum val)
      resL <- sumAndTraceInOrder leftCh
      resR <- sumAndTraceInOrder rightCh
      pure $ val : resL <> resR

-------------------------------------------------------------------------------

-- + 10. Monad `Reader` (1,75 балла)

-- | Зададим свой Reader
newtype Reader' r a = Reader' { runReader' :: r -> Identity a }

---------------------------------------

-- 10.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' w a -> Reader' w b
    fmap f aR = Reader' bR
      where
        bR w = Identity $ f . runIdentity $ runReader' aR w

instance Applicative (Reader' r) where
    pure :: a -> Reader' r a
    pure x = Reader' $ \_ -> Identity x

    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    (<*>) fR aR = Reader' $ \w -> bR w
      where
        f w = runIdentity $ runReader' fR w
        bR w = Identity $ f w . runIdentity $ runReader' aR w

instance Monad (Reader' r) where
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    (>>=) aR k = Reader' $ \w -> runReader' (k . runIdentity $ runReader' aR w) w

---------------------------------------

-- 10.b Реализуйте инастанс класса `MonadReader` для `Reader` (ask | reader, local) (0,25 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#t:MonadReader

instance MonadReader r (Reader' r) where
    ask :: Reader' r r
    ask = Reader' Identity

    local :: (r -> r) -> Reader' r a -> Reader' r a
    local f aR = Reader' $ \w -> runReader' aR (f w)

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
eval (Primary (Val x)) = return . Just $ x
eval (Primary (Var x)) = reader $ \r -> M.lookup x r
eval (Binary left' right') = do
    resLeft <- eval left'
    resRight <- eval right'
    return $ do
        x <- resLeft
        y <- resRight
        return $ x + y

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
--   В качестве результата вычисления всего списка договоримся возвращать результат вычисления 
--   последнего выражения в списке
--

evalStmts :: [Stmt] -> Reader' Environment (Maybe Int)
evalStmts []                      = return Nothing
evalStmts [x]                     = eval (expr x)
evalStmts (x : xs) = do
    maybeVal <- eval (expr x)
    _ <- ask
    local (maybe id (M.insert (name x)) maybeVal) (evalStmts xs)

-- evalStmts [] = pure Nothing
-- evalStmts [x] = eval (expr x)
-- evalStmts (x : xs) =
--   let v = eval (expr x) in
--     Reader' $ \r -> runReader' (evalStmts xs) (
--       case runIdentity $ runReader' v r of
--         Just vv -> M.insert (name x) vv r
--         Nothing -> r
--     )


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
