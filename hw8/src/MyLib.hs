{-# LANGUAGE InstanceSigs, FlexibleInstances, MultiParamTypeClasses #-}

--  MultiParamTypeClasses позволяет нам определять такие классы типов:
--     instance (Monoid w) => MonadWriter w (Writer' w) where
-- или instance MonadReader r (Reader' r) where
-- то есть определяеть классы типов с несолькими параметрами

module MyLib where

{- cabal:
    build-depends: base, mtl, containers
-}
{- install for ghci:
    > cabal install mtl
    > ghci
    >> :set -package mtl
-}
import           Control.Applicative (ZipList(..))
import           Control.Monad.Writer.Strict
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Monoid()
import           Data.Maybe(fromMaybe)

-------------------------------------------------------------------------------

-- 1. Travserable (1,5 балла)

---------------------------------------

-- 1.a Реализуйте инстансы Traversable для Maybe и списка (без док-ва законов) (0,5 балла)

-- Все закоментированно, потому что данный инстанс уже Defined in `Data.Traversable'
-- Maybe:
-- instance Traversable Maybe where
--   traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
--   traverse _ Nothing  = pure Nothing
--   traverse f (Just a) = Just <$> f a

--   sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
--   sequenceA Nothing   = pure Nothing
--   sequenceA (Just fa) = Just <$> fa

-- List:
-- instance Traversable List where
--   traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
--   traverse _ Nil         = pure Nil
--   traverse f (Cons x xs) = Cons
--                       <$> f x
--                       <*> traverse f xs
--   sequenceA :: Applicative f => List (f a) -> f (List a)
--   sequenceA Nil         = pure Nil
--   sequenceA (Cons x xs) = Cons
--                       <$> x
--                       <*> sequenceA xs
---------------------------------------

-- 1.b Реализуйте `traverse` через `sequenceA` и `sequenceA` через `traverse` (0,5 балла)
--
-- На эти функции ругается hlint, значит все сделано правильно

traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA . fmap f

sequenceA' :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA' = traverse id

---------------------------------------

-- 1.c В чем разница между Traversable и Functor? Между Traversable и Foldable? (0,5 балла)

-- Traversable позволяет не просто применять функцию к значению в контексте, как это делает Functor,
-- но и собирать полученный результат в какой-нибудь другой контекст.

-- Foldable предоставляет способ сворачивать структуры данных в одно значение,
-- в то время как Traversable предоставляет способ применять функцию, возвращающую структуру данных, ко всей структуре данных.
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
transpose ::  [[a]] -> [[a]]
transpose = (.) getZipList (traverse ZipList)

-- Не очень понял вопрос. Не знал, что нужны какие-то дополнительные условия, чтобы использовать его как функцию, а не оператор
-------------------------------------------------------------------------------

-- 4. Для чего нужен класс типов MonadFail? (0,25 балла)
-- Для обработки ошибок в контексте Монад, то есть для случаев,
-- когда монадические вычисления могут завершиться неудачей или ошибкой.
--
-- Например, пусть есть функция, ищущая элемент в списке и возвращающая Either String a,
-- где String - это сообщение об ошибке, а 'a' - тип элемента.

-- Например, это может случиться при обработке случаев в if then else, тогда может прийти значение, 
-- которое не удовлетворяет нашим условиям и мы вернем сообщение об ошибке
--
-- В do нотации мы на прямую не указываем, что может пойти не так (зафейлится), 
-- но если что-то все таки пойдет не так, то вызовется fail
-- 
-- MonadFail можно использовать не в do- нотации. Это можно делать в тех случаях, когда мы реализуем свой тип данных и реализуем для него инстанс монады,
-- тогда можно обрабатывать ошибки так, как мы захотим


-------------------------------------------------------------------------------

-- | 5. Сделайте (WithData d) монадой и не забудьте про 'MonadFail'.
--       Опишите словами, какой эффект получился у созданной вами монады.
--       Без описания задание не засчитывается (0,5 балла)
--
newtype WithData d a = WithData { runWithData :: d -> a }

instance Functor (WithData d) where
  fmap :: (a -> b) -> WithData d a -> WithData d b
  fmap f (WithData g) = WithData (f . g)

instance Applicative (WithData d) where
  pure :: a -> WithData d a
  pure  = (.) WithData const

  (<*>) :: WithData d (a -> b) -> WithData d a -> WithData d b
  WithData f <*> WithData g = WithData (\d -> f d (g d))

instance Monad (WithData d) where
  return :: a -> WithData d a
  return = pure

  (>>=) :: WithData d a -> (a -> WithData d b) -> WithData d b
  WithData g >>= f = WithData (\d -> runWithData (f (g d)) d)

instance MonadFail (WithData d) where
  fail :: String -> WithData d a
  fail message = WithData (\_ -> error message)

-- Эта монада WithData d представляет собой монаду,
-- которая инкапсулирует вычисление, зависящее от некоторых данных типа d.
-- Эффект заключается в хранении какой-нибудь функции
-- При вызове runWithData в нее передаются значения, а она возвращает какой-нибудь результат

-------------------------------------------------------------------------------

-- 6. Do-нотация (1 балл)

---------------------------------------

-- | 6.a Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы (0,5 балла)
--
-- Тут много ворнингов к функциям в do-нотации, смысла их исправлять нет, потому что это часть задания
--
fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
fromDo11 aM bM = do
    a <- fmap (+ 10) aM

    -- в одном 'let'-выражении внутри do-нотации можно писать несколько связываний.
    -- в обычных 'let'-выражениях это тоже работает
    let aL = [a, a, a]
        a  = a + length aL

    return a  -- hlint ругается на функцию из задания, очевидно, ничего исправлять не стал

    bM
    [a, b, c] <- Just aL

    b <- fmap (<> "abcd") bM

    pure (c, b)

myFromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
myFromDo11 aM bM =
    fmap (+ 10) aM >>= \a ->
    let aL = [a, a, a]
    in
        return a >>
        bM >>
        case aL of
            [a, b, c] -> fmap (<> "abcd") bM >>= \b ->
                            pure (c, b)
            _ -> Nothing

-- тут много ворнингов на shadows the existing binding,
-- я их намерено не исправляю, чтобы сделать полное воспроизведение функции
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

myFromDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
myFromDo12 isL cM =
    isL >>= \curI ->
    tail isL >>= \nextI ->
    if nextI > curI
        then let a = curI + nextI
            in tail (tail isL) >>= \nextNextI ->
                [cM] >>= \chM ->
                case (curI, nextI, nextNextI, chM) of
                    (0, 0, 0, Just ch) -> pure (ch, a)
                    _ -> fail ""
        else pure ('0', 0)


-- очень тяжко объединять требования к отсутсвию ворнингов и замечаний hlint в этом задании.
-- потому что они есть к исходнику и, соответственно, к нашим версиям.
-- я их не исправляю, чтобы соответсвовать требованиям задания
-------------------------------------------------------------------------------

-- 7. С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. Дублирования нужно убрать за счёт дополнительного условия в do-нотации (0,5 балла)
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = do
  c <- [1..]
  b <- [1..c]
  a <- [1..b]
  guard $ a*a + b*b == c*c
  guard $ a < b
  return (a, b, c)


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

returnExample :: ReturnableCalculation Int Int 
returnExample = do
    let a' = 40
        b' = 2

    _ <- realReturn $ a' + b'

    let newA = 0

    if newA == (0 :: Int)
      then pure 200
      else realReturn 0

newtype ReturnableCalculation a b = ReturnableCalculation {runCalculation :: Either a b}
  deriving(Show, Eq)

instance Functor (ReturnableCalculation a) where
  fmap :: (b -> c) -> ReturnableCalculation a b -> ReturnableCalculation a c
  fmap f (ReturnableCalculation calculation) =
    ReturnableCalculation $ fmap f calculation -- точно, можно использовать просто fmap

instance Applicative (ReturnableCalculation a) where
  pure :: b -> ReturnableCalculation a b
  pure val = ReturnableCalculation (Right val)

  (<*>) :: ReturnableCalculation a (b -> c) -> ReturnableCalculation a b -> ReturnableCalculation a c
  ReturnableCalculation func <*> ReturnableCalculation val =
    ReturnableCalculation $ func <*> val

instance Monad (ReturnableCalculation a) where
  (>>=) :: ReturnableCalculation a b -> (b -> ReturnableCalculation a c) -> ReturnableCalculation a c
  ReturnableCalculation calculation >>= f =
    ReturnableCalculation $ calculation >>= (.) runCalculation f


realReturn :: a -> ReturnableCalculation a b
realReturn = ReturnableCalculation . Left
-------------------------------------------------------------------------------

-- 9. Monad `Writer` (1,5 балла)

-- | Зададим свой Writer
newtype Writer' w a = Writer' { runWriter' :: (Identity a, w) }
  deriving (Show, Eq)

---------------------------------------

-- 9.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Writer' w) where
  fmap :: (a -> b) -> Writer' w a -> Writer' w b
  fmap f (Writer' (value, log')) = Writer' (f <$> value, log')

instance Monoid w => Applicative (Writer' w) where
  pure :: a -> Writer' w a
  pure val = Writer' (pure val, mempty)

  (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
  Writer' (func, logFunc) <*> Writer' (val, logVal) = Writer' (func <*> val, logFunc <> logVal)

instance Monoid w => Monad (Writer' w) where
  (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
  Writer' (value, logValue) >>= f =
    let Writer' (newValue, logNew) = f $ runIdentity value
    in Writer' (newValue, logValue <> logNew)

---------------------------------------

-- 9.b Реализуйте инастанс класса `MonadWriter` для `Writer` (tell | writer, listen, pass) (0,5 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Strict.html#t:MonadWriter

instance (Monoid w) => MonadWriter w (Writer' w) where
    tell :: w -> Writer' w ()
    tell w = Writer' (Identity (), w)

    listen :: Writer' w a -> Writer' w (a, w)
    listen (Writer' (Identity a, w)) = Writer' (Identity (a, w), w)

    pass :: Writer' w (a, w -> w) -> Writer' w a
    pass (Writer' (Identity (a, f), w)) = Writer' (Identity a, f w)


-- Почему нужно было определять `Writer' w a`, а не `Writer' a w`?
-- В типе данных Writer' w a, параметр w представляет моноид, 
-- который используется для накопления лога, а параметр a представляет значение,
-- которое вычисляется.
-- Но инстанс принимает Writer' w (частичное применение), поэтому в случае a w 
-- мы не сможем определить инстанс, потому что w это лог, а не значение 
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
sumAndTraceInOrder (Node value l r) = do
  leftResult  <- sumAndTraceInOrder l
  tell (Sum value)
  rightResult <- sumAndTraceInOrder r
  return (leftResult ++ [value] ++ rightResult)

-------------------------------------------------------------------------------

-- 10. Monad `Reader` (1,75 балла)

-- | Зададим свой Reader
newtype Reader' r a = Reader' { runReader' :: r -> Identity a }

---------------------------------------

-- 10.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' r a -> Reader' r b
    fmap f (Reader' ra) = Reader' $ \r -> Identity (f (runIdentity (ra r)))

instance Applicative (Reader' r) where
    pure :: a -> Reader' r a
    pure a = Reader' $ \_ -> Identity a

    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    (Reader' rf) <*> (Reader' ra) = Reader' $ \r ->
        let f = runIdentity (rf r)
            a = runIdentity (ra r)
        in Identity (f a)

instance Monad (Reader' r) where
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    (Reader' ra) >>= f = Reader' $ \r ->
        let a = runIdentity (ra r)
        in runReader' (f a) r


---------------------------------------

-- 10.b Реализуйте инастанс класса `MonadReader` для `Reader` (ask | reader, local) (0,25 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#t:MonadReader

instance MonadReader r (Reader' r) where
    ask :: Reader' r r
    ask = Reader' $ \r -> Identity r

    local :: (r -> r) -> Reader' r a -> Reader' r a
    local f (Reader' ra) = Reader' $ \r -> ra (f r)

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
eval (Primary (Var varName)) = do
  asks (M.lookup varName)
eval (Primary (Val value)) = return $ Just value
eval (Binary leftExpr rightExpr) = do
  maybeLeft  <- eval leftExpr
  maybeRight <- eval rightExpr
  return $ (+)
        <$> maybeLeft
        <*> maybeRight

-- пустые строки добавлял для лучшей читабельности кода, их можно убрать на функциональность они не влияют

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
evalStmts (x : xs) = do
  case xs of
    [] -> eval $ expr x 
    _ -> do
      env <- ask 
      let newValue   = runIdentity $ runReader' (eval $ expr x) env 
      let updatedEnv = M.insert (name x) (fromMaybe 0 newValue) env 
      local (const updatedEnv) $ evalStmts xs

-- hlint предложил использовать fromMaybe
-- Found:
--   maybe 0 id
-- Perhaps:
--   Data.Maybe.fromMaybe 0

-- | Пример запуска вычисления списка утверждений
--
testEvalStmts :: Maybe Int
testEvalStmts = runIdentity $ runReader' (evalStmts [x, y, z, xx, w]) M.empty
  where
    x, y, z, xx, w :: Stmt
    x  = Stmt "x" $ Primary . Val $ 2                                     -- x = 2
    y  = Stmt "y" $ Primary . Val $ 3                                     -- y = 3
    z  = Stmt "z" $ Binary (Primary . Var $ "x") (Primary . Var $ "y")    -- z = 5
    xx = Stmt "x" $ Binary (Primary . Var $ "x") (Primary . Var $ "x")   -- xx = 4
    w  = Stmt "w" $ Binary (Primary . Var $ "z") (Primary . Var $ "x")    -- w = 9

-------------------------------------------------------------------------------
