{-# LANGUAGE TupleSections, InstanceSigs #-}
module MyLib where

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import qualified System.Random as R
import           Data.List (find)
import           Control.Monad (when)
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))
import Data.Map.Internal (alter)

-------------------------------------------------------------------------------

-- 1. Writer: Брандмауэр (3 балла)

--    Напишите очень простой брандмауэр: он фильтрует пакеты на основе базы правил,
--    соответствующих IP-адресам отправителя и получателя.
--    Основная задача брандмауэра -- фильтрация пакетов, но мы хотели бы, чтобы он также вел
--    журнал своей работы (логи)

-- | Формат логов
--
data Entry = Log {
      count :: Int     -- количество таких сообщений подряд, идущих друг за другом
    , msg   :: String  -- сообщение
    } deriving (Show, Eq)

-- | Добавляет сообщение в лог
--
logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

-- | Задайте тип, выражающий IP-адрес
--
newtype IP = IPAddress String
  deriving (Show, Eq)

data Action = Accept | Reject
  deriving (Eq)

instance Show Action where
  show :: Action -> String
  show Accept = "Packet accept"
  show Reject = "Packet reject"

data Rule = Rule {
      action :: Action
    , source :: IP
    , destination :: IP
    } deriving (Show, Eq)

data Packet = Packet {
      pSource :: IP
    , pDestination :: IP
    } deriving (Eq)

instance Show Packet where
  show :: Packet -> String
  show packet = " input: " <> show (pSource packet) <> " output: " <> show (pDestination packet)

-- | Возвращает первое правило, действующее на пакет
--
match :: [Rule] -> Packet -> Maybe Rule
match rules (Packet ps pd) = find (\ (Rule _ s d) -> s == ps && d == pd) rules

-- | Фильтрует 1 пакет
-- 
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do
  let maybeRule = match rules packet
  case maybeRule of
    Just (Rule act _ _) -> do
      logMsg $ show act <> show packet
      pure (Just packet)
    _                   -> do
      logMsg $ "No rules for packet" <> show packet
      pure Nothing

-- Немного усложним задачу: теперь мы хотим объединить дублирующиеся последовательные записи в журнале.
-- Ни одна из существующих функций не позволяет изменять результаты предыдущих этапов вычислений,
-- но мы можем использовать прием "отложенного протоколирования", чтобы добавлять запись в лог только
-- после получения новой записи, которая не совпадает с предыдущими.

-- | Объединяет одинаковые записи в конце лога
--   Эта функция использует [Entry] как в качестве типа лога, так и в качестве типа результата.
--   При объединении двух одинаковых сообщений результатом будет только сообщение с увеличенным счетчиком.
--   При объединении двух разных сообщений, в лог записывается первое сообщение, а второе возвращается в качестве результата.
--
mergeEntries :: [Entry] -> [Entry] -> Writer [Entry] [Entry]
mergeEntries message []         = writer ([], message)
mergeEntries []      message    = writer ([], message)
mergeEntries [l1]  log2@(l2:t2) = do
  if msg l1 == msg l2
    then writer ([], Log (count l1 + count l2) (msg l1) : t2)
    else writer ([l1], log2)
mergeEntries _       _          = error "Uncorrect logs!"

-- | Применяет входную функцию к списку значений, чтобы получить список Writer.
--   Затем запускает каждый Writer и объединяет результаты.
--   Результатом работы функции является Writer, значение которого -- список всех значений из всех Writer,
--   а лог -- результат слияния всех логов всех Writer
--
-- 'initial' -- изначальное значение лога
--
groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial _ [] _ = writer ([], initial)
groupSame initial merge (x:xs) fn = do
  let (f, logF) = runWriter $ fn x
      (v, logT) = runWriter $ groupSame initial merge xs fn
      (res, newLog) = runWriter $ merge logF logT
  writer (f : v, res <> newLog)

-- | Фильтрует список пакетов и возвращает список отфильтрованных пакетов и логи
--
filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll rules packets = groupSame [] mergeEntries packets (filterOne rules) <&> catMaybes

-------------------------------------------------------------------------------

-- 2. Reader: Инстанцирование шаблонов (2 балла)

-- Рассмотрим проблему инстанцирования шаблонов, содержащих подстановки переменных и включенные шаблоны.
-- Используя монаду Reader, мы можем поддерживать окружение/контекст всех известных шаблонов и всех известных переменных.
-- Когда встречается подстановка переменной, мы можем использовать функцию asks для поиска значения переменной в окружении.
-- Если в шаблон включены определения новых переменных, мы будем должны изменить окружение, добавив в него эти перменные;
-- продолжить работу на измененном окружении можно с помощью функции local

data Template = Text String
              | Var Template
              | Quote Template
              | Include Template [Definition]
              | Compound [Template]
  deriving (Show, Eq)

data Definition = Definition Template Template
  deriving (Show, Eq)

-- | Окружение -- это имя шаблона в шаблон и имя переменной в переменную
--
data Environment = Env {
      templs :: M.Map String Template
    , vars   :: M.Map String String
    } deriving (Show, Eq)

-- | Ищет переменную в окружении
--
lookupVar :: String -> Environment -> Maybe String
lookupVar v e = M.lookup v (vars e)

-- | Ищет шаблон в окружении
--
lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate t e = M.lookup t (templs e)

-- | Добавляет новые переменные в окружение
-- 
addDefs :: M.Map String String -> Environment -> Environment
addDefs v e = Env (templs e) (M.union v (vars e))

-- | Резолвит (подставляет все неизвестные) шаблон и выдает в качестве ответа
--   пару (имя шаблона, значение)
--
resolveDef :: Definition -> Reader Environment (String, String)
resolveDef (Definition temp1 temp2) = do
  resolveTemp1 <- resolve temp1
  resolveTemp2 <- resolve temp2
  pure (resolveTemp1, resolveTemp2)

-- | Резолвит (подставляет все неизвестные) шаблон в строку
--
resolve :: Template -> Reader Environment String
resolve (Text tempName) = pure tempName -- имя наблона/переменной
-- находим имя переменной, возвращаем её значение из Env vars
resolve (Var varTemp) = do
  varName <- resolve varTemp
  maybeVarVal <- asks $ lookupVar varName
  pure $ maybe "" show maybeVarVal
-- находим имя шаблона, возвращаем его "значение" из Env templs 
resolve (Quote qTemp) = do
  qName <- resolve qTemp
  maybeQVal <- asks $ lookupTemplate qName
  maybe (pure "") resolve maybeQVal
-- находим имя шаблона, возвращаем его "значение" из окружения
-- + записываем в окружение новые шаблоны (из definition) : имя tempL, значение tempR 
-- их как (String, String) возьмём из resolveDef
resolve (Include temp defTemps) = do
  incName <- resolve temp
  maybeIncVal <- asks $ lookupTemplate incName
  case maybeIncVal of
    Just incVal -> do
      templsPairs <- traverse resolveDef defTemps               -- находим (имя, значение) для всех Definition
      local (addDefs $ M.fromList templsPairs) $ resolve incVal -- добавляем их в окружение
      -- возвращаем зачение __нашего__ шаблона
    Nothing     -> pure ""
-- находим имя шаблона, возвращаем сконкатенированные "значения" его вложенных шаблонов
resolve (Compound temps) = do
  tempsRes <- traverse resolve temps
  pure $ mconcat tempsRes

-- Функция для запуска новых Template'ов на изменённом окружении
mutResolve :: [Definition] -> Reader Environment String -> Reader Environment String
mutResolve defTemps t = do
  templsPairs <- traverse resolveDef defTemps
  local (addDefs $ M.fromList templsPairs) t

-------------------------------------------------------------------------------

-- 3. State: Генерация случайного значения кастомного типа (1 балл)

-- Чистый функциональный язык не может обновлять значения на месте (почему?) 
--    потому что одной переменной не может быть присвоено два разных значения
-- Распространенной идиомой для имитации таких вычислений с сохранением состояния является
-- "прохождение" параметра состояния через последовательность функций
-- Рассмотрим пример

data MyType = MT Int Bool Char Int
  deriving Show

-- Чтобы запустить: `makeRandomValue (mkStdGen 23)`

makeRandomValue :: R.StdGen -> (MyType, R.StdGen)
makeRandomValue g =
    let (n,g1) = R.randomR (1, 100) g
        (b,g2) = R.random g1
        (c,g3) = R.randomR ('a', 'z') g2
        (m,g4) = R.randomR (-n, n) g3
    in (MT n b c m, g4)

-- Этот подход работает, но такой код сложен в сопровождении, может содержать ошибки и быть грязным (что делает его грязным?)
--    потому что функция randomR возвращает псевдо-случайное число 
--    а значит может выдавать разный результат на одинаковых входных данных

-- Монада State скрывает потоковую передачу состояния (как вы понимаете слова "потоковая передача состояния") внутри операции >>=,
-- делая код проще для написания, чтения и модификации.


-- | Возвращает случайное значение и обновляет состояние генератора случайных чисел
--
getAny :: (R.Random a) => State R.StdGen a
getAny = state R.random

doubleGetAny :: State R.StdGen (Int, Int)
doubleGetAny = do
  r1 <- (getAny :: State R.StdGen Int)
  r2 <- (getAny :: State R.StdGen Int)
  pure (r1, r2)

-- state :: (s -> (a, s)) -> m a
-- random :: RandomGen g => g -> (a, g)
-- | Аналогична getAny, но генерирует значение в границах
--
getOne :: (R.Random a) => (a, a) -> State R.StdGen a
getOne bounds = state $ R.randomR bounds

doubleGetOne :: (Int, Int) -> State R.StdGen (Int, Int)
doubleGetOne bound = do
  r1 <- (getOne bound :: State R.StdGen Int)
  r2 <- (getOne bound :: State R.StdGen Int)
  pure (r1, r2)

-- | Используя монаду State с StdGen в качестве состояния, мы можем генерировать случаные значения
--   заданного типа, не передавая состояния генератора случайных чисел в коде вручную
--
makeRandomValueST :: R.StdGen -> (MyType, R.StdGen)
makeRandomValueST = runState st
  where
    st :: State R.StdGen MyType
    st = do
      n <- getOne (1, 100)
      b <- getAny
      c <- getOne ('a', 'z')
      m <- getOne (-n, n)
      return (MT n b c m)

-------------------------------------------------------------------------------

-- 4. "Императивное" программирование (2 балла)

---------------------------------------

-- | 4.1. Реализуйте функцию `whileM_`, исполняющую заданное вычисление,
--        пока первое вычисление возвращает 'True'
--
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ flagM actionM = do
  flag <- flagM
  when flag $ do
    _ <- actionM            -- вычисляем  действие, при этом результат нам не важен
    whileM_ flagM actionM

  -- на `when` hlint сказал исправить. + добавлены ограничения на класс типов:
  -- 1. when :: Applicative f => Bool -> f () -> f ()
  -- 2. add (Monad m) to the context of the type signature in a stmt of a 'do' block: flag <- flagM

---------------------------------------

-- | 4.2. Реализуйте функцию `forM_`, являющуюуся аналогом цикла for.
--        Перед входом в цикл вычисляется init, в итерациях цикла вычисляется body, 
--        в конце каждой итерации цикла вычисляется nextIter, итерации идут, 
--        пока выполняется условие cond
--
forM_ :: Monad m => (m (), m Bool, m ()) -> m a -> m ()
forM_ (initM, condM, nextIterM) bodyM = do
  _ <- initM
  helper -- нужен, чтобы не считалось каждый раз init
  where
    helper = do
      cond <- condM
      when cond $ do
        _ <- bodyM
        _ <- nextIterM
        helper

---------------------------------------

-- | Окружение / контекст: имя переменной в значение
--
type Context = M.Map String Integer

-- 4.3. Реализуйте следующие функции

-- | Задаёт значение переменной. 
--   Если переменная есть в контексте, перезаписывает её значение.
--
setVar :: String -> Integer -> State Context ()
setVar name val = state (\e -> ((), M.insert name val e))

-- | Увеличивает значение переменной. 
--   Если её нет в контексте, то кидает ошибку.
--
incVar :: String -> Integer -> State Context ()
incVar name delta = do
  env <- get
  put $ alter (maybe (error "Variable not in Context!") (Just . (+) delta)) name env

-- | Достаёт из контекста значение заданной переменной.
--   Если переменной нет в контексте, то кидает ошибку.
--
getVar :: String -> State Context Integer
getVar name = do
  env <- get
  maybe (error "Variable not in Context!") return $ M.lookup name env

---------------------------------------

-- 4.4. Перепешите один в один (насколько это возможно) представленный ниже плюсовый код, 
--      используя монаду 'State' и функции, которые вы реализовали выше. 
--      Напишите 3 теста, проверяющие, что ваша реализация работает корректно

{- 
int fib(int n) {
    int prev = 0;
    int cur = 1;

    for (int i = 0; i < n; i = i + 1) {
        int c = cur;
        cur = prev + cur;
        prev = c;
    }

    return cur
} 
-}

fib :: Integer -> State Context Integer
fib n = do
  setVar "prev" 0
  setVar "cur" 1
  forM_ (setVar "i" 0, condition, incVar "i" 1) body
  getVar "cur"
    where
      condition :: State Context Bool
      condition = do
        i <- getVar "i"
        pure $ i < n

      body :: State Context ()
      body = do
        c <- getVar "cur"
        setVar "c" c

        prev <- getVar "prev"
        incVar "cur" prev

        setVar "prev" c


-------------------------------------------------------------------------------

-- 5*. State и Either (2 балла)
--     Создайте свою монаду StateWithError, обладающую сразу двумя эффектами:
--      a) эффектом монады 'State'  (изменяемое состояние);
--      b) эффектои монады 'Either' (возможность завершения вычисления ошибкой)

-- | Задайте тип
--
newtype StateWithError s a b = MyState {runMyState :: s -> (Either a b, s) }

-- Реализуйте инстанс Monad для StateWithError

instance Functor (StateWithError s a) where
  fmap :: (b -> c) -> StateWithError s a b -> StateWithError s a c
  fmap f st = MyState $ \s' -> (fmap f . fst $ runMyState st s', s')

instance Applicative (StateWithError s a) where
  pure :: b -> StateWithError s a b
  pure x = MyState (Right x,)

  (<*>) :: StateWithError s a (b -> c) -> StateWithError s a b -> StateWithError s a c
  (<*>) fSt xSt = MyState $ \s' -> (fst (runMyState fSt s') <*> fst (runMyState xSt s'), s')

instance Monad (StateWithError s a) where
  (>>=) :: StateWithError s a b -> (b -> StateWithError s a c) -> StateWithError s a c
  (>>=) xSt k = MyState $ \s' -> case runMyState xSt s' of
    (Left x,  s'') -> (Left x, s'')
    (Right x, s'') -> runMyState (k x) s''

-- Привидите пример использования

-------------------------------------------------------------------------------
