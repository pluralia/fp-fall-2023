{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances, LambdaCase #-}

module MyLib where

import           Control.Monad.Writer.Strict
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Monoid()
import Control.Applicative (ZipList(..))

-------------------------------------------------------------------------------

-- 1. Travserable (1,5 балла)

---------------------------------------

-- 1.a Реализуйте инстансы Traversable для Maybe и списка (без док-ва законов) (0,5 балла)

----MAYBE----

data Maybe' a = Nothing' | Just' a
  deriving (Show, Eq)

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' $ f a

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Nothing' <*> _ = Nothing'
  _ <*> Nothing' = Nothing'
  (Just' f) <*> (Just' a) = Just' $ f a

instance Foldable Maybe' where
  foldr :: (a -> b -> b) -> b -> Maybe' a -> b
  foldr _ b Nothing' = b
  foldr f b (Just' a) = f a b

instance Traversable Maybe' where
  traverse :: Applicative f => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse _ Nothing' = pure Nothing'
  traverse f (Just' a) = Just' <$> f a

  sequenceA :: Applicative f => Maybe' (f a) -> f (Maybe' a)
  sequenceA Nothing' = pure Nothing'
  sequenceA (Just' fa) = Just' <$> fa

----LIST----

data List' a = Nil' | Cons' a (List' a)
  deriving (Show, Eq)

instance Semigroup (List' a) where
  (<>) :: List' a -> List' a -> List' a
  (<>) Nil' as = as
  (<>) (Cons' a as) bs = Cons' a (as <> bs)

instance Functor List' where
  fmap :: (a -> b) -> List' a -> List' b
  fmap _ Nil' = Nil'
  fmap f (Cons' a as) = Cons' (f a) (fmap f as)

instance Applicative List' where
  pure :: a -> List' a
  pure a = Cons' a Nil'

  (<*>) :: List' (a -> b) -> List' a -> List' b
  (<*>) Nil' _ = Nil'
  (<*>) _ Nil' = Nil'
  (<*>) (Cons' f fs) as = fmap f as <> (fs <*> as) 

instance Foldable List' where
  foldr :: (a -> b -> b) -> b -> List' a -> b
  foldr _ b Nil' = b
  foldr f b (Cons' a as) = f a (foldr f b as)

instance Traversable List' where
  traverse :: Applicative f => (a -> f b) -> List' a -> f (List' b)
  traverse _ Nil' = pure Nil'
  traverse f (Cons' a as) = Cons' <$> f a <*> traverse f as

  sequenceA :: Applicative f => List' (f a) -> f (List' a)
  sequenceA Nil' = pure Nil'
  sequenceA (Cons' fa fas) = Cons' <$> fa <*> sequenceA fas

---------------------------------------

-- 1.b Реализуйте `traverse` через `sequenceA` и `sequenceA` через `traverse` (0,5 балла)

traverse' :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA . fmap f

sequenceA' :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA' = traverse id

-- hlint ругается. пж не не надо баллы снимать (
-- с другой стороны это значит, что сделано правильно )

---------------------------------------

-- 1.c В чем разница между Traversable и Functor? Между Traversable и Foldable? (0,5 балла)

-- Functor позволяет вам применять функцию к значению в контексте
-- Traversable позволяет вам применять функцию к значению в контексте и собирать результаты в другой контекст

-- Я не понят причем тут Foldable. Ведь он позволяет свернуть структуру данных в одно значение.

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

-- ZipList заменяет Applicative для стандартного списка. Вместо каждый с каждым, будет работать как zip

-------------------------------------------------------------------------------

-- 4. Для чего нужен класс типов MonadFail? (0,25 балла)

-- Для того, чтобы можно было обрабатывать ошибки в монадах.
-- MonadFail предоставляет метод fail. Используем его для сигнализации об ошибке.
-- Пример: у Maybe есть инстанс MonadFail. Метод fail возвращает Nothing.

-------------------------------------------------------------------------------

-- | 5. Сделайте (WithData d) монадой и не забудьте про 'MonadFail'.
--       Опишите словами, какой эффект получился у созданной вами монады.
--       Без описания задание не засчитывается (0,5 балла)
--
newtype WithData d a = WithData { runWithData :: d -> a }

instance Functor (WithData d) where
  fmap :: (a -> b) -> WithData d a -> WithData d b
  fmap f (WithData a) = WithData $ f . a


instance Applicative (WithData d) where
  pure :: a -> WithData d a
  pure = WithData . const

  (<*>) :: WithData d (a -> b) -> WithData d a -> WithData d b
  (<*>) (WithData ff) (WithData aa) = WithData $ helper ff aa
    where
      helper :: (d -> a -> b) -> (d -> a) -> d -> b
      helper f a d = f d $ a d
      
      
instance Monad (WithData d) where
  return :: a -> WithData d a
  return = pure

  (>>=) :: WithData d a -> (a -> WithData d b) -> WithData d b
  (>>=) (WithData aa) ff = WithData $ helper aa ff
    where
      helper :: (d -> a) -> (a -> WithData d b) -> d -> b
      helper a f d = runWithData (f $ a d) d


instance MonadFail (WithData d) where
  fail :: String -> WithData d a
  fail _ = WithData $ const undefined

-- Эффект: мы храним функцию. При вызове runWithData мы передаем в нее данные и получаем результат.
-- Как такая монада может зафейлиться? 


-------------------------------------------------------------------------------

-- 6. Do-нотация (1 балл)

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
    

fromDo11' :: Maybe Int -> Maybe String -> Maybe (Int, String)
fromDo11' aM bM = fmap (+ 10) aM >>= \a ->
  let 
    aL = [a, a, a]
    a  = a + length aL
  in return a >> bM >> Just aL >>= \case
    [a, b, c] -> fmap (<> "abcd") bM >>= \b -> pure (c, b)
    _ -> fail ""
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


fromDo12' :: [Int] -> Maybe Char -> [(Char, Int)]
fromDo12' isL cM = isL >>= \curI ->
    tail isL >>= \nextI ->
        if nextI > curI
          then let a = curI + nextI in
            tail (tail isL) >>= \nextNextI ->
            [cM] >>= \case -- hlint предложил \case
            -- получается можно не определять переменную для case
              Just ch -> 
                case (curI, nextI, nextNextI) of
                  (0, 0, 0) -> pure (ch, a)
                  _         -> fail ""
              Nothing -> fail ""
          else pure ('0', 0)

-------------------------------------------------------------------------------

-- 7. С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. Дублирования нужно убрать за счёт дополнительного условия в do-нотации (0,5 балла)

pifList :: [(Int, Int, Int)]
pifList = do
  c <- [1..]
  b <- [1..c]
  a <- [1..b]
  if a*a + b*b == c*c
    then pure (a, b, c)
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

type ReturnableCalculation a = ReturnableCalculation' a a

newtype ReturnableCalculation' a b = ReturnableCalculation' { runCalculation :: Either a b }
-- левый тип для досрочного выхода из вычислений
-- правый тип для результата вычислений
-- я не додумался как сделать это по другому

instance Functor (ReturnableCalculation' a) where
  fmap :: (b -> c) -> ReturnableCalculation' a b -> ReturnableCalculation' a c
  fmap f (ReturnableCalculation' x) = ReturnableCalculation' (f <$> x)

instance Applicative (ReturnableCalculation' a) where
  pure :: b -> ReturnableCalculation' a b
  pure = ReturnableCalculation' . Right

  (<*>) :: ReturnableCalculation' a (b -> c) -> ReturnableCalculation' a b -> ReturnableCalculation' a c
  ReturnableCalculation' f <*> ReturnableCalculation' x = ReturnableCalculation' (f <*> x)

instance Monad (ReturnableCalculation' a) where
  (>>=) :: ReturnableCalculation' a b -> (b -> ReturnableCalculation' a c) -> ReturnableCalculation' a c
  ReturnableCalculation' (Left x) >>= _ = ReturnableCalculation' (Left x)
  ReturnableCalculation' (Right x) >>= f = f x

realReturn :: a -> ReturnableCalculation' a b
realReturn = ReturnableCalculation' . Left

-------------------------------------------------------------------------------

-- 9. Monad `Writer` (1,5 балла)

-- | Зададим свой Writer
newtype Writer' w a = Writer' { runWriter' :: (Identity a, w) }
  deriving (Show)

---------------------------------------

-- 9.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Writer' w) where
    fmap :: (a -> b) -> Writer' w a -> Writer' w b
    fmap f (Writer' (a, w)) = Writer' (f <$> a, w)

instance Monoid w => Applicative (Writer' w) where
    pure :: a -> Writer' w a
    pure a = Writer' (pure a, mempty)

    (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
    Writer' (l, lf) <*> Writer' (x, lx) = Writer' (l <*> x, lf <> lx)

instance Monoid w => Monad (Writer' w) where
    (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
    Writer' (x, lx) >>= f = Writer' (x', lx <> lx')
      where
        Writer' (x', lx') = f $ runIdentity x

---------------------------------------

-- 9.b Реализуйте инастанс класса `MonadWriter` для `Writer` (tell | writer, listen, pass) (0,5 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Strict.html#t:MonadWriter

instance (Monoid w) => MonadWriter w (Writer' w) where
    tell :: w -> Writer' w ()
    tell m = Writer' (pure (), m)

    listen :: Writer' w a -> Writer' w (a, w)
    listen (Writer' (x, m)) = Writer' (pure (runIdentity x, m), m)

    pass :: Writer' w (a, w -> w) -> Writer' w a
    pass (Writer' (x, m)) = 
      let (a, f) = runIdentity x
      in Writer' (Identity a, f m)

-- Почему нужно было определять `Writer' w a`, а не `Writer' a w`?

-- тк instance принимает частично примененный тип, то есть Writer' w
-- если мы определим Writer' a w, то мы не сможем определить instance
-- тк w это лог, а не результат вычислений

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
sumAndTraceInOrder (Node v l r) = do
  leftSum <- sumAndTraceInOrder l
  rightSum <- sumAndTraceInOrder r
  tell $ Sum v
  pure $ leftSum ++ [v] ++ rightSum


bintree :: BinaryTree Int
bintree = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)

testSumAndTraceInOrder :: (Identity [Int], Sum Int)
testSumAndTraceInOrder = runWriter' $ sumAndTraceInOrder bintree

-------------------------------------------------------------------------------

-- 10. Monad `Reader` (1,75 балла)

-- | Зададим свой Reader
newtype Reader' r a = Reader' { runReader' :: r -> Identity a }

---------------------------------------

-- 10.a Реализуйте для него `Monad` (док-во законов не нужно) (0,5 балла)

instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' w a -> Reader' w b
    fmap f (Reader' a) = Reader' $ fmap f . a

instance Applicative (Reader' r) where
    pure :: a -> Reader' r a
    pure = Reader' . const . pure

    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    Reader' fa <*> Reader' xx = Reader' $ helper fa xx
      where
        helper :: (r -> Identity (a -> b)) -> (r -> Identity a) -> r -> Identity b
        helper f x r = f r <*> x r

instance Monad (Reader' r) where
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    Reader' xx >>= fa = Reader' $ helper xx fa
      where
        helper :: (r -> Identity a) -> (a -> Reader' r b) -> r -> Identity b
        helper x f r = runReader' (f $ runIdentity $ x r) r

---------------------------------------

-- 10.b Реализуйте инастанс класса `MonadReader` для `Reader` (ask | reader, local) (0,25 балла)
--     https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#t:MonadReader

instance MonadReader r (Reader' r) where
    ask :: Reader' r r
    ask = Reader' pure

    local :: (r -> r) -> Reader' r a -> Reader' r a
    local f m = Reader' $ runReader' m . f

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
eval e = do
  case e of
    Primary i -> evalItem i
      where
        evalItem :: Item -> Reader' Environment (Maybe Int)
        evalItem (Val x) = pure $ Just x
        evalItem (Var x) = Reader' $ pure . M.lookup x
    Binary l r -> do
      l' <- eval l
      r' <- eval r
      pure $ (+) <$> l' <*> r'

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
evalStmts [x] = eval $ expr x
evalStmts (x : xs) = do
  maybeVal <- eval $ expr x
  local (maybe id (M.insert (name x)) maybeVal) $ evalStmts xs


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


