{-# LANGUAGE FlexibleInstances, InstanceSigs, MultiParamTypeClasses #-}
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
import           Control.Monad (guard)
import           Control.Applicative (ZipList(..))


-- Hlint ругается на задание 1b, но это нормально из-за смысла самого задания, не обращаем внимание
-- а еще немного на 10с задание, но я чтобы все красиво и понятно в do-нотации выглядело так оставил, не стал менять

-- Ворнинги есть (много, но все не по делу) из-за 7 задания, из-за использования оператора возведения в степень ^ и floor 
-- там, наверное, если везде прописать что-то типо :: Double/Int, то он перестанет ругаться,
-- но у меня не вышло сделать это так, чтобы прям все ворнинги убрать
-- на работу функции они никак не влияют, просто компилятору не нравится, что нет явного указания типов
-- также есть ворнинги по 8 и 10 заданию в примерах, которые изначально были прописаны в дз
-- (в функциях returnExample и testEvalExpr) их я тоже не трогал, т.к. не я писал и кто я такой, чтобы код преподавателя переписывать :)



-------------------------------------------------------------------------------

-- 1. Travserable (1,5 балла)

---------------------------------------

-- 1.a Реализуйте инстансы Traversable для Maybe и списка (без док-ва законов) (0,5 балла)

-- instance Traversable Maybe where
--   traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
--   traverse _ Nothing = pure Nothing
--   traverse f (Just a) = Just <$> f a

-- instance Traversable [] where
--   traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
--   traverse _ [] = pure []
--   traverse f (x:xs) = (:) <$> f x <*> traverse f xs

---------------------------------------

-- 1.b Реализуйте `traverse` через `sequenceA` и `sequenceA` через `traverse` (0,5 балла)

traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA . fmap f

sequenceA' :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA' = traverse id

---------------------------------------

-- 1.c В чем разница между Traversable и Functor? Между Traversable и Foldable? (0,5 балла)

{- РАЗНИЦА МЕЖДУ Traversable и Functor
Основная функция для Functor - это fmap
Она позволяет применять функцию к значению, находящемуся внутри назовем это "контейнера", сохраняя его структуру
fmap :: Functor f => (a -> b) -> f a -> f b

Основная функция для Traversable - это traverse 
Она позволяет применять функцию к каждому элементу в структуре и собирать результаты в контейнер
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

Таким образом, основное различие заключается в том, что Functor позволяет применять функцию к значению в контексте, 
сохраняя структуру контейнера, в то время как Traversable позволяет применять функцию ко всем элементам структуры 
и собирать результаты внутри контейнера, сохраняя структуру, но изменяя значения внутри неё
-}

{-РАЗНИЦА МЕЖДУ Traversable и Foldable
Основная функция для Foldable - это foldMap
Она позволяет сворачивать структуру данных в одно значение (ну, не то, чтобы прям одно, но смысл примерно такой), используя заданную операцию
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

Основная функция для Traversable - это traverse 
Она позволяет применять функцию к каждому элементу в структуре и собирать результаты в контейнер
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

Т.е. видим два основных отличия:
1.
Foldable применяет функцию, чтобы свернуть структуру в одно значение
Traversable применяет функцию к каждому элементу структуры, сохраняя структуру, но изменяя значения внутри

2.
Foldable возвращает одно значение, свернутое из структуры
Traversable возвращает структуру того же типа, что и исходная, но с измененными значениями внутри
-}
-------------------------------------------------------------------------------

-- | 2. Реализуйте `rejectWithNegatives`, которая возвращает исходный список, обернутый в Just,
--       если в нем нет отрицательных элементов, и Nothing в противном случае (0,5 балла)
--
rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives xs = if any (< 0) xs then Nothing else Just xs
--   where
--     deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
--     deleteIfNegative x = if x < 0 then Nothing else Just x

-------------------------------------------------------------------------------

-- | 3. Рассмотрим представление матриц в виде вложенных списков, в которых внутренние списки являются строками.
--       Используйте Traversable для реализации транспонирования матриц (0,5 балла)
--
-- transpose :: [[a]] -> [[a]]
-- transpose [] = []
-- transpose ([]:_) = []
-- transpose xs = fmap head xs : transpose (fmap tail xs)

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

-------------------------------------------------------------------------------

-- 4. Для чего нужен класс типов MonadFail? (0,25 балла)

-- Чтобы контроллировать, как мы падаем (обрабатываем ошибки) 
-- Он есть дефолтный, но если делать свои кастомные штуки, то нужно самому его реализовывать

-------------------------------------------------------------------------------

-- | 5. Сделайте (WithData d) монадой и не забудьте про 'MonadFail'.
--       Опишите словами, какой эффект получился у созданной вами монады.
--       Без описания задание не засчитывается (0,5 балла)
--
newtype WithData d a = WithData { runWithData :: d -> a }

instance MonadFail (WithData d) where
  fail _ = WithData $ \_ -> error "Ошибка в монаде WithData: не удалось выполнить операцию"

instance Functor (WithData d) where
  fmap f w = WithData $ \d -> f (runWithData w d)

instance Applicative (WithData d) where
  pure x = WithData $ const x
  f <*> x = WithData $ \d -> runWithData f d (runWithData x d)

instance Monad (WithData d) where
  return = pure
  w >>= f = WithData $ \d -> runWithData (f (runWithData w d)) d

-- WithData принимает некое окружение d и возвращает значение, которое на этом окружении посчитали, при вызове runWithData

-------------------------------------------------------------------------------

-- 6. Do-нотация (1 балл)

---------------------------------------

-- | 6.a Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы (0,5 балла)
--
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

-- fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
-- fromDo11 aM bM =
--   fmap (+ 10) aM >>= \a ->

--   let aL = [a, a, a]
--       a'  = a + length aL
--   in 
--     return a >>
--     bM >>
--     (Just aL >>= \[a, b, c] ->

--     fmap (<> "abcd") bM >>= \b ->

--     pure (c, b))

---------------------------------------

-- | 6.b Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы (0,5 балла)
--
-- fromDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
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

-- fromDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
-- fromDo12 isL cM =
--   isL >>= \curI ->
--   tail isL >>= \nextI ->
--   if nextI > curI
--     then
--       let a = curI + nextI
--       in
--         tail (tail isL) >>= \nextNextI ->
--         [cM] >>= \x -> 
--         case x of
--           (Just ch) ->
--             case (curI, nextI, nextNextI) of
--               (0, 0, 0) -> pure (ch, a)
--               _         -> fail "" 
--           _         -> fail ""
--     else pure ('0', 0)


-------------------------------------------------------------------------------

-- 7. С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. Дублирования нужно убрать за счёт дополнительного условия в do-нотации (0,5 балла)

pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriples n = do
  a <- [1..n]  
  b <- [1..a] 
  c <- [1..b]
  if a * a == b * b + c * c
    then return (a, b, c)
    else fail "Пифагоровы штаны не во все стороны равны получились"

-- pythagoreanTriples :: [(Integer, Integer, Integer)]
-- pythagoreanTriples = do
--   a <- [1..]
--   b <- [a..]
--   let c = floor $ sqrt (fromIntegral (a^2 + b^2))
--   guard (a^2 + b^2 == c^2)
--   return (a, b, c)

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
    let a :: Int
        a = 40
        b :: Int
        b = 2

    realReturn $ a + b

    let aa :: Int
        aa = 0

    if aa == 0
      then pure 200
      else realReturn 0

data ReturnableCalculation a = RealReturn a | IgnoredReturn
    deriving (Eq, Show)

instance Functor ReturnableCalculation where
    fmap :: (a -> b) -> ReturnableCalculation a -> ReturnableCalculation b
    fmap f (RealReturn a) = RealReturn (f a)
    fmap _ IgnoredReturn = IgnoredReturn

instance Applicative ReturnableCalculation where
    pure :: a -> ReturnableCalculation a
    pure = RealReturn

    (<*>) :: ReturnableCalculation (a -> b) -> ReturnableCalculation a -> ReturnableCalculation b
    RealReturn f <*> RealReturn a = RealReturn (f a)
    RealReturn _ <*> IgnoredReturn = IgnoredReturn
    IgnoredReturn <*> RealReturn _ = IgnoredReturn
    IgnoredReturn <*> IgnoredReturn = IgnoredReturn

instance Monad ReturnableCalculation where
    (>>=) :: ReturnableCalculation a -> (a -> ReturnableCalculation b) -> ReturnableCalculation b
    RealReturn a >>= f = f a
    IgnoredReturn >>= _ = IgnoredReturn

realReturn :: a -> ReturnableCalculation a
realReturn = RealReturn

-------------------------------------------------------------------------------

-- 9. Monad `Writer` (1,5 балла)

-- | Зададим свой Writer
newtype Writer' w a = Writer' { runWriter' :: (Identity a, w) }
  deriving (Show, Eq)

---------------------------------------

-- 9.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Writer' w) where
    fmap :: (a -> b) -> Writer' w a -> Writer' w b
    fmap f (Writer' (Identity a, w)) = Writer' (Identity (f a), w)

instance Monoid w => Applicative (Writer' w) where
    pure :: a -> Writer' w a
    pure a = Writer' (Identity a, mempty)

    (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
    Writer' (Identity f, w1) <*> Writer' (Identity a, w2) = Writer' (Identity (f a), w1 `mappend` w2)

instance Monoid w => Monad (Writer' w) where
    (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
    Writer' (Identity a, w) >>= f = let Writer' (Identity b, w') =  f a 
                                    in Writer' (Identity b, w `mappend` w')

---------------------------------------

-- 9.b Реализуйте инстанс класса `MonadWriter` для `Writer` (tell | writer, listen, pass) (0,5 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Strict.html#t:MonadWriter

instance (Monoid w) => MonadWriter w (Writer' w) where
    tell :: w -> Writer' w ()
    tell w = Writer' (Identity (), w)

    listen :: Writer' w a -> Writer' w (a, w)
    listen (Writer' (Identity a, w)) = Writer' (Identity (a, w), w)

    pass :: Writer' w (a, w -> w) -> Writer' w a
    pass (Writer' (Identity (a, f), w)) = Writer' (Identity a, f w)

-- Почему нужно было определять `Writer' w a`, а не `Writer' a w`?

-- На самом деле не обязательно было определять именно так, можно и наоборот, вопрос просто в том, что
-- именно для нас важнее - логи (накапливаемое значение) или результат 
-- (То есть, Writer' w a представляет вычисление, которое генерирует некоторое значение типа a и также может порождать некоторые данные типа w)


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
sumAndTraceInOrder tree = do
  case tree of
    Leaf -> pure []
    Node val lleft rright -> do
      -- Рекурсивно обходим левое поддерево
      leftValues <- sumAndTraceInOrder lleft

      -- Записываем текущее значение в контекст суммы
      tell (Sum val)

      -- Рекурсивно обходим правое поддерево
      rightValues <- sumAndTraceInOrder rright

      -- Конкатенируем значения из левого поддерева, текущего узла и правого поддерева
      pure $ leftValues ++ [val] ++ rightValues

-------------------------------------------------------------------------------

-- 10. Monad `Reader` (1,75 балла)

-- | Зададим свой Reader
newtype Reader' r a = Reader' { runReader' :: r -> Identity a }

---------------------------------------

-- 10.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' w a -> Reader' w b
    fmap f (Reader' ra) = Reader' $ \r -> Identity $ f $ runIdentity (ra r)

instance Applicative (Reader' r) where
    pure :: a -> Reader' r a
    pure a = Reader' $ \_ -> Identity a

    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    (Reader' rab) <*> (Reader' ra) = Reader' $ \r -> Identity $ runIdentity (rab r) (runIdentity (ra r))

instance Monad (Reader' r) where
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    (Reader' ra) >>= f = Reader' $ \r -> Identity $ runIdentity $ runReader' (f (runIdentity (ra r))) r

---------------------------------------

-- 10.b Реализуйте инастанс класса `MonadReader` для `Reader` (ask | reader, local) (0,25 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#t:MonadReader

instance MonadReader r (Reader' r) where
    ask :: Reader' r r
    ask = Reader' $ \r -> Identity r

    local :: (r -> r) -> Reader' r a -> Reader' r a
    local f (Reader' ra) = Reader' $ \r -> Identity $ runIdentity (ra (f r))

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
eval (Primary (Val val)) = pure (Just val)
eval (Primary (Var var)) = do
    env <- ask
    pure (M.lookup var env)
eval (Binary leftExpr rightExpr) = do
    leftVal <- eval leftExpr
    rightVal <- eval rightExpr
    pure $ (+) <$> leftVal <*> rightVal

-- | Пример запуска вычисления выражения
--
testEvalExpr :: Maybe Int -- ожидаем `Just 5`
testEvalExpr = runIdentity $ runReader' (eval eexpr) env
  where
    env :: Environment
    env = M.fromList [("x", 3)]

    eexpr :: Expr
    eexpr = Binary (Primary . Val $ 2) (Primary . Var $ "x")

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
evalStmts []                        = return Nothing
evalStmts [Stmt _ exprr]            = eval exprr
evalStmts ((Stmt nname exprr) : xs) = do
    maybeVal <- eval exprr
    _ <- ask
    local (maybe id (M.insert nname) maybeVal) (evalStmts xs)

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
