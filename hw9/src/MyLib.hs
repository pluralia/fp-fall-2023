{-# LANGUAGE InstanceSigs, TupleSections #-}

module MyLib where
import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified System.Random as R -- cabal install --lib  random
--import           Debug.Trace
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
    } deriving (Eq, Show)

-- | Добавляет сообщение в лог
--
logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

-- | Задайте тип, выражающий IP-адрес
--
data IP = IP Int Int Int Int
  deriving (Eq)

instance Show IP where
  show :: IP -> String
  show (IP a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

data Action = Accept | Reject
  deriving (Show, Eq)

data Rule = Rule {
      action :: Action
    , source :: IP
    , destination :: IP
    } deriving (Show, Eq)

data Packet = Packet {
      pSource :: IP
    , pDestination :: IP
    } deriving Eq

instance Show Packet where
  show :: Packet -> String
  show (Packet src dst) = "Packet from " ++ show src ++ " to " ++ show dst
  

-- | Возвращает первое правило, действующее на пакет
--
match :: [Rule] -> Packet -> Maybe Rule
match rules packet = find (\rule -> source rule == pSource packet && destination rule == pDestination packet) rules

-- | Фильтрует 1 пакет
-- 
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = 
  case match rules packet of
    Nothing -> do
      logMsg $ show packet ++ " is rejected"
      return Nothing
    Just rule -> do
      case action rule of
        Accept -> do
          logMsg $ show packet ++ " is accepted"
        Reject -> do
          logMsg $ show packet ++ " is rejected"
      return $ Just packet

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
mergeEntries [] [] = return []
mergeEntries (e : es) [] = tell [e] >> return es
mergeEntries [] (e' : es') = mergeEntries [e'] es'
mergeEntries (e : es) (e' : es') =
  if msg e == msg e'
    then 
      let e'' = Log (count e + count e') (msg e) in mergeEntries (e'' : es) es'
    else 
      tell [e] >> mergeEntries ([e', e] ++ es) es'


-- | Применяет входную функцию к списку значений, чтобы получить список Writer.
--   Затем запускает каждый Writer и объединяет результаты.
--   Результатом работы функции является Writer, значение которого -- список всех значений из всех Writer,
--   а лог -- результат слияния всех логов всех Writer
--
-- 'initial' -- изначальное значение лога
--
groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial merge xs fn =
  do
    let
      (ys, logs) = runWriter $ mapM fn xs
      (_, logs') = runWriter $ merge initial logs
    tell logs'
    return ys

-- | Фильтрует список пакетов и возвращает список отфильтрованных пакетов и логи
--
filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll rules packets = catMaybes <$> groupSame [] mergeEntries packets (filterOne rules)

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

-- | Окружение -- это имя шаборна в шаблон и имя переменной в переменную
--
data Environment = Env {
      templs :: M.Map String Template
    , vars   :: M.Map String String
    } deriving (Show, Eq)

-- | Ищет переменную в окружении
--
lookupVar :: String -> Environment -> Maybe String
lookupVar var = M.lookup var . vars

-- | Ищет шаблон в окружении
--
lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate templ = M.lookup templ . templs

-- | Добавляет новые переменные в окружение
-- 
addDefs :: M.Map String String -> Environment -> Environment
addDefs new env = env { vars = M.union new (vars env) }

-- | Резолвит (подставляет все неизвестные) шаблон и выдает в качестве ответа
--   пару (имя шаборна, значение)
--
resolveDef :: Definition -> Reader Environment (String, String)
resolveDef (Definition templ val) = do
  tmp <- resolve templ
  v <- resolve val
  return (tmp, v)

-- | Резолвит (подставляет все неизвестные) шаблон в строку
--
resolve :: Template -> Reader Environment String
resolve (Text s) = return s

resolve (Var templ) = do
  varName  <- resolve templ
  varValue <- asks (lookupVar varName)
  return $ fromMaybe "" varValue

resolve (Quote templ) = do
  name <- resolve templ
  body <- asks (lookupTemplate name)
  resolve $ fromMaybe (Text "") body

resolve (Include templ defs) = do
  env <- ask
  name <- resolve templ
  body <- resolve $ fromMaybe (Text "") $ lookupTemplate name env
  do
    resolvedDefs <- mapM resolveDef defs
    local (addDefs (M.fromList resolvedDefs)) $ resolve $ Text body
     
resolve (Compound ts) = do
  resolvedTempls <- mapM resolve ts
  pure $ mconcat resolvedTempls
    

-------------------------------------------------------------------------------

-- 3. State: Генерация случайного значения кастомного типа (1 балл)

-- Чистый функциональный язык не может обновлять значения на месте (почему?)

-- По определению)

-- Распространенной идиомой для имитации таких вычислений с сохранением состояния является
-- "прохождение" параметра состояния через последовательность функций
-- Рассмотрим пример

data MyType = MT Int Bool Char Int
  deriving (Show, Eq)

-- Чтобы запустить: `makeRandomValue (R.mkStdGen 23)`

makeRandomValue :: R.StdGen -> (MyType, R.StdGen)
makeRandomValue g =
  let (n,g1) = R.randomR (1, 100) g
      (b,g2) = R.random g1
      (c,g3) = R.randomR ('a', 'z') g2
      (m,g4) = R.randomR (-n, n) g3
  in (MT n b c m, g4)

-- Этот подход работает, но такой код сложен в сопровождении, может содержать ошибки и быть грязным (что делает его грязным?)

-- Грязным делает то, что мы не можем отделить чистый код от кода, который изменяет состояние. Наверное

-- Монада State скрывает потоковую передачу состояния (как вы понимаете слова "потоковая передача состояния") внутри операции >>=,
-- делая код проще для написания, чтения и модификации.


-- | Возвращает случайное значение и обновляет состояние генератора случайных чисел
--
getAny' :: (R.Random a) => State R.StdGen a
getAny' = state R.random

-- | Аналогична getAny, но генерирует значение в границах
--
getOne' :: (R.Random a) => (a, a) -> State R.StdGen a
getOne' bounds = state $ R.randomR bounds

-- | Используя монаду State с StdGen в качестве состояния, мы можем генерировать случаные значения
--   заданного типа, не передавая состояния генератора случайных чисел в коде вручную
--
makeRandomValueST :: R.StdGen -> (MyType, R.StdGen)
makeRandomValueST = runState $ do
  n <- getOne' (1, 100)
  b <- getAny'
  c <- getOne' ('a', 'z')
  m <- getOne' (-n, n)
  return $ MT n b c m

makeRandomValueST' :: R.StdGen -> (MyType, R.StdGen)
makeRandomValueST' = runState $ do
  b <- getAny'
  n <- getOne' (1, 100)
  c <- getOne' ('a', 'z')
  m <- getOne' (-n, n)
  return $ MT n b c m

-- Я думаю, что если состояние генератора не обновляется, то порядок вызова функций не важен
-- и makeRandomValueST' и makeRandomValueST будут возвращать одинаковые результаты
-- но
--ghci> makeRandomValueST (R.mkStdGen 23)
--(MT 25 False 'l' 0,    StdGen {unStdGen = SMGen 6031929740467884236 16778118630780010967})
--ghci> makeRandomValueST' (R.mkStdGen 23)
--(MT 25 False 'm' (-22),StdGen {unStdGen = SMGen 7700555183397424885 16778118630780010967})
-------------------------------------------------------------------------------

-- 4. "Императивное" программирование (2 балла)

---------------------------------------

-- | 4.1. Реализуйте функцию `whileM_`, исполняющую заданное вычисление,
--        пока первое вычисление возвращает 'True'
--
whileM_' :: Monad m => m Bool -> m a -> m ()
whileM_' cond body = do
  c <- cond
  when c $ body >> whileM_' cond body

---------------------------------------

-- | 4.2. Реализуйте функцию `forM_`, являющуюуся аналогом цикла for.
--        Перед входом в цикл вычисляется init, в итерациях цикла вычисляется body, 
--        в конце каждой итерации цикла вычисляется nextIter, итерации идут, 
--        пока выполняется условие cond
--
forM_' :: Monad m => (m (), m Bool, m ()) -> m a -> m ()
forM_' (init', cond, nextIter) body = do
  init'
  whileM_' cond $ body >> nextIter

---------------------------------------

-- | Окружение / контекст: имя переменной в значение
--
type Context = M.Map String Int

-- 4.3. Реализуйте следующие функции

-- | Задаёт значение переменной. 
--   Если переменная есть в контексте, перезаписывает её значение.
--
setVar :: String -> Int -> State Context ()
setVar s i = do
  ctx <- get
  put $ M.insert s i ctx

-- | Увеличивает значение переменной. 
--   Если её нет в контексте, то кидает ошибку.
--
incVar :: String -> Int -> State Context ()
incVar s i = do
  ctx <- get
  put $ M.alter (fmap (+ i)) s ctx

-- | Достаёт из контекста значение заданной переменной.
--   Если переменной нет в контексте, то кидает ошибку.
--
getVar :: String -> State Context Int
getVar s = do
  ctx <- get
  case M.lookup s ctx of
    Just val -> return val
    Nothing -> error $ "No variable " ++ s ++ " found"

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
        prev = cur;
    }

    return cur
} 
-}

fib :: Int -> State Context Int
fib n = do
  setVar "prev" 0
  setVar "cur" 1
  forM_' (setVar "i" 0, getVar "i" >>= (\i -> return $ i < n), incVar "i" 1) $ do
    setVar "c" =<< getVar "cur"
    --setVar "cur" =<< (+) <$> getVar "prev" <*> getVar "cur"
    incVar "cur" =<< getVar "prev" -- верхнее больше похоже на тот плюсовый код. Это больше похоже на cur += prev
    setVar "prev" =<< getVar "c"
  getVar "cur"


-------------------------------------------------------------------------------

-- 5*. State и Either (2 балла)
--     Создайте свою монаду StateWithError, обладающую сразу двумя эффектами:
--      a) эффектом монады 'State'  (изменяемое состояние);
--      b) эффектои монады 'Either' (возможность завершения вычисления ошибкой)

-- | Задайте тип
--
newtype StateWithError a
  = StateWithError {runStateWithError :: Int -> (Either String a, Int)}

-- Реализуйте инстанс Monad для StateWithError

instance Functor StateWithError where
  fmap :: (a -> b) -> StateWithError a -> StateWithError b
  fmap f (StateWithError g) = StateWithError $ \s -> 
    let (res, s') = g s in
      (f <$> res, s')

instance Applicative StateWithError where
  pure :: a -> StateWithError a
  pure a = StateWithError (Right a,)

  (<*>) :: StateWithError (a -> b) -> StateWithError a -> StateWithError b
  (StateWithError f) <*> (StateWithError g) = StateWithError $ \s -> 
    let (res, s') = f s in
      case res of
        Left err -> (Left err, s')
        Right f' -> 
          let (res', s'') = g s' in
            case res' of
              Left err -> (Left err, s'')
              Right g' -> (Right $ f' g', s'')

instance Monad StateWithError where
  (>>=) :: StateWithError a -> (a -> StateWithError b) -> StateWithError b
  (StateWithError f) >>= g = StateWithError $ \s -> 
    let (res, s') = f s in
      case res of
        Left err -> (Left err, s')
        Right a -> runStateWithError (g a) s'


-- | Возвращает текущее состояние
--
getSt :: StateWithError Int
getSt = StateWithError $ \s -> (Right s, s)

-- | Задает новое состояние
--
putSt :: Int -> StateWithError ()
putSt s = StateWithError $ const (Right (), s)

-- | Возвращает текущее состояние и завершает вычисление с ошибкой
--
throwErrorSt :: String -> StateWithError a
throwErrorSt err = StateWithError $ const (Left err, 0)

-- Пример использования:

-- | Возвращает текущее состояние, увеличенное на 1
--
getStPlus1 :: StateWithError Int
getStPlus1 = do
  s <- getSt
  putSt $ s + 1
  return s

-- | Возвращает текущее состояние, увеличенное на 1, если оно не превышает 10
--
getStPlus1IfNotTooBig :: StateWithError Int
getStPlus1IfNotTooBig = do
  s <- getStPlus1
  if s <= 10
    then return s
    else throwErrorSt "Number is too big"

-------------------------------------------------------------------------------
