{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE LambdaCase #-} --hlint посоветовала
-- нужен для того, чтобы использовать \case

module MyLib where
import Data.List (find)

{- cabal:
    build-depends: base, mtl, containers
-}
{- install for ghci:
    > cabal install mtl
    > ghci
    >> :set -package mtl
-}

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Control.Monad()
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map.Strict as M
import qualified System.Random as R -- cabal install --lib  random

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
  deriving (Show, Eq)

data Rule = Rule {
      action' :: Action
    , source :: IP
    , destination :: IP
    } deriving (Show, Eq)

data Packet = Packet {
      pSource :: IP
    , pDestination :: IP
    } deriving (Show, Eq)

-- | Возвращает первое правило, действующее на пакет
--
match :: [Rule] -> Packet -> Maybe Rule
match rules packet = find (\r -> source r == pSource packet && destination r == pDestination packet) rules


-- | Фильтрует 1 пакет
-- 
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do
  case match rules packet of
    Just rule -> logAndReturn $ if action' rule == Accept then " accepted" else " rejected"
    Nothing -> logAndReturn " rejected"
  where
    -- | Функция 'logAndReturn' логгирует сообщение о статусе пакета и возвращает соответствующий результат.
    logAndReturn status = do
      logMsg $ show packet ++ status
      return $ if status == " rejected" then Nothing else Just packet



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
mergeEntries (x : xs) [] =
  tell [x] >> return xs
mergeEntries [] (log' : rest) =
  mergeEntries [log'] rest
mergeEntries (x : xs) (log' : rest) =
  if msg x == msg log'
    then
      let x' = Log (count x + count log') (msg x)
      in mergeEntries (x' : xs) rest
    else
      tell [x] >> mergeEntries ([log', x] ++ xs) rest


-- добавил примеров для запуска на списках длины 2 и больше
-- добавил Ваше тест

-- | Применяет входную функцию к списку значений, чтобы получить список Writer.
--   Затем запускает каждый Writer и объединяет результаты.
--   Результатом работы функции является Writer, значение которого -- список всех значений из всех Writer,
--   а лог -- результат слияния всех логов всех Writer
--
-- 'initial' -- изначальное значение лога
--
groupSame :: (Monoid accumulation) => accumulation -> (accumulation -> accumulation -> Writer accumulation accumulation) -> [item] -> (item -> Writer accumulation result) -> Writer accumulation [result]
groupSame initialAccumulation mergeFunction items processFunction =
  let
    (results, logs) = runWriter $ mapM processFunction items
    (_, updatedLogs) = runWriter $ mergeFunction initialAccumulation logs
  in
    tell updatedLogs >> return results


-- Оборачивание лога в список (с использованием tell mergedLog) позволяет накапливать лог для каждого элемента списка.

-- Пример использования groupSame

-- Функция для слияния логов, просто конкатенирующая строки
mergeLogs :: String -> String -> Writer String String
mergeLogs log1 log2 = tell (log1 ++ log2) >> return log1

-- Функция, преобразующая целое число в Writer с удвоенным значением и строковым логом
doubleAndLog :: Int -> Writer String Int
doubleAndLog x = tell ("Doubling " ++ show x) >> return (x * 2)

-- Пример списка значений
values :: [Int]
values = [1, 2, 3, 4, 5]

-- Использование groupSame для применения doubleAndLog ко всем элементам списка
-- с слиянием логов mergeLogs и начальным значением лога "Initial Log:\n"
resultWriter :: Writer String [Int]
resultWriter = groupSame "Initial Log:\n" mergeLogs values doubleAndLog

-- | Фильтрует список пакетов и возвращает список отфильтрованных пакетов и логи
filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll rules packets = 
  let processPacket = filterOne rules
      initialAccumulation = []
      mergeFunction = mergeEntries
  in do
    results <- groupSame initialAccumulation mergeFunction packets processPacket
    return $ catMaybes results




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
lookupVar var env = M.lookup var (vars env)

-- | Ищет шаблон в окружении
--
lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name env = M.lookup name (templs env)

-- | Добавляет новые переменные в окружение
-- 
addDefs :: M.Map String String -> Environment -> Environment
addDefs newDefs env = env { vars = M.union (vars env) newDefs }

-- | Резолвит (подставляет все неизвестные) шаблон в строку
--
resolve :: Template -> Reader Environment String
resolve (Text textContent) = return textContent

resolve (Var (Text varName)) = asks $ fromMaybe "" . lookupVar varName

resolve (Var templateVar) = resolve templateVar

resolve (Quote templ) = resolve templ >>= \name ->
  asks (lookupTemplate name) >>= \body ->
    resolve $ fromMaybe (Text "") body

resolve (Include templ defs) = do
  env <- ask
  name <- resolve templ
  body <- resolve $ fromMaybe (Text "") $ lookupTemplate name env
  resolvedDefs <- mapM resolveDef defs
  local (addDefs (M.fromList resolvedDefs)) $ resolve $ Text body

resolve (Compound ts) = mconcat <$> mapM resolve ts


-- Резолвит (подставляет все неизвестные) шаблон и выдает в качестве ответа
-- пару (имя шаблона, значение)
resolveDef :: Definition -> Reader Environment (String, String)
resolveDef (Definition templateName value) = do
  env <- ask
  let resolvedValue = runReader (resolve value) env
      getTemplateName :: Template -> String
      getTemplateName (Text name)    = name
      getTemplateName (Var template) = runReader (resolve template) env -- точно! добавил
      getTemplateName _              = ""
  return (getTemplateName templateName, resolvedValue)


-------------------------------------------------------------------------------

-- 3. State: Генерация случайного значения кастомного типа (1 балл)

-- Чистый функциональный язык не может обновлять значения на месте (почему?)
-- Распространенной идиомой для имитации таких вычислений с сохранением состояния является
-- "прохождение" параметра состояния через последовательность функций

-- Чистый функциональный язык характеризуется отсутствием побочных эффектов и изменяемого состояния.
-- В чистой функциональности значения являются неизменяемыми, и любые изменения создают новые значения вместо модификации существующих.
-- Это свойство обеспечивает прозрачность ссылок, делая код более предсказуемым и легким в понимании.

-- Рассмотрим пример

data MyType = MT Int Bool Char Int
  deriving (Show, Eq)

-- Чтобы запустить: `makeRandomValue (mkStdGen 23)`

makeRandomValue :: R.StdGen -> (MyType, R.StdGen)
makeRandomValue g =
    let (n,g1) = R.randomR (1, 100) g
        (b,g2) = R.random g1
        (c,g3) = R.randomR ('a', 'z') g2
        (m,g4) = R.randomR (-n, n) g3
    in (MT n b c m, g4)

-- Этот подход работает, но такой код сложен в сопровождении, может содержать ошибки и быть грязным (что делает его грязным?)
-- Монада State скрывает потоковую передачу состояния (как вы понимаете слова "потоковая передача состояния") внутри операции >>=,
-- делая код проще для написания, чтения и модификации.

-- Мне кажется, что Потоковая передача состояния означает, что состояние передается между функциями в виде параметра.
-- Вместо того, чтобы изменять состояние на месте, каждая функция принимает текущее состояние и возвращает новое состояние вместе с результатом вычисления.
-- Это обеспечивает иммутабельность данных и изолирует изменения состояния внутри функциональных вычислений.
--
-- Прохождение состояния является подходом, который может привести к нескольким вложенным "колбасным" функциям,
-- что делает код сложным для понимания и сопровождения. Это усложняет отслеживание состояния и может сделать код менее читаемым.
-- Также, такой код подвержен ошибкам, связанным с передачей состояния между функциями.

-- | Возвращает случайное значение и обновляет состояние генератора случайных чисел
--
getAny :: (R.Random a) => State R.StdGen a
getAny = state R.random

-- | Аналогична getAny, но генерирует значение в границах
--
getOne :: (R.Random a) => (a, a) -> State R.StdGen a
getOne bounds = state $ R.randomR bounds

-- Функции getAny и getOne используют монаду State StdGen для передачи состояния генератора случайных чисел.
-- Мне кажется, что обновление состояния будет автоматически обработано.

-- | Используя монаду State с StdGen в качестве состояния, мы можем генерировать случаные значения
--   заданного типа, не передавая состояния генератора случайных чисел в коде вручную
--
makeRandomValueST :: R.StdGen -> (MyType, R.StdGen)
makeRandomValueST =
  runState $
    getOne (1, 100) >>= \n ->
      getOne (False, True) >>= \b ->
        getOne ('a', 'z') >>= \c ->
          getOne (-n, n) >>= \m ->
            return (MT n b c m)

-------------------------------------------------------------------------------

-- 4. "Императивное" программирование (2 балла)

---------------------------------------

-- | 4.1. Реализуйте функцию `whileM_`, исполняющую заданное вычисление,
--        пока первое вычисление возвращает 'True'
--
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ condition action = condition >>= \shouldContinue ->
  when shouldContinue
    $ action >> whileM_ condition action

-- Здесь whileM_ используется для уменьшения счетчика в монаде State Int до нуля.
-- example представляет собой цикл, который продолжается, пока счетчик больше нуля.
-- Вычисления запускаются с начальным состоянием, например 5,
-- и результат представляет собой пару из значения () (поскольку whileM_ возвращает () в каждой итерации) и конечного значения счетчика, то есть 0.

type CounterMonad = State Int

decrementCounter :: CounterMonad ()
decrementCounter = modify (\x -> x - 1)

-- Функция проверки условия (счетчик больше 0)
checkCondition :: CounterMonad Bool
checkCondition = gets (> 0)

-- Пример использования whileM_
example :: CounterMonad ()
example = whileM_ checkCondition decrementCounter
---------------------------------------

-- | 4.2. Реализуйте функцию `forM_`, являющуюуся аналогом цикла for.
--        Перед входом в цикл вычисляется init, в итерациях цикла вычисляется body, 
--        в конце каждой итерации цикла вычисляется nextIter, итерации идут, 
--        пока выполняется условие cond
--
forM_' :: Monad m => (m (), m Bool, m ()) -> m a -> m ()
forM_' (init', cond, nextIter) body =
  init' >> checkCondition'
  where
    checkCondition' = do
      shouldContinue <- cond
      when shouldContinue
       $ body
        >> nextIter
         >> checkCondition'

-- Эту функцию я тестирую непосредственно в функции fib
---------------------------------------

-- | Окружение / контекст: имя переменной в значение
--
type Context = M.Map String Int

-- 4.3. Реализуйте следующие функции

-- | Задаёт значение переменной. 
--   Если переменная есть в контексте, перезаписывает её значение.
--
setVar :: String -> Int -> State Context ()
setVar varName value = do
  context <- get
  put $ M.insert varName value context

-- | Увеличивает значение переменной. 
--   Если её нет в контексте, то кидает ошибку.
--
incVar :: String -> Int -> State Context ()
incVar varName increment = do
  modify $ M.alter updateVar varName -- через alter
  where
    updateVar :: Maybe Int -> Maybe Int
    updateVar (Just oldValue) = Just (oldValue + increment)
    updateVar Nothing = error $ "Variable not found: " ++ varName  

-- | Достаёт из контекста значение заданной переменной.
--   Если переменной нет в контексте, то кидает ошибку.
--
getVar :: String -> State Context Int
getVar varName = do
  context <- get
  case M.lookup varName context of
    Just value -> return value
    Nothing    -> error $ "Variable not found: " ++ varName

---------------------------------------

-- 4.4. Перепешите один в один (насколько это возможно) представленный ниже плюсовый код, 
--      используя монаду 'State' и функции, которые вы реализовали выше. 
--      Напишите 3 теста, проверяющие, что ваша реализация работает корректно

{- 
int fib(int n) {
    int prev = 0;
    int cur = 1;

    for (int i = 0; i < n; i = i + 1) {
        int temp = cur;
        cur = prev + cur;
        prev = temp;          Теперь используется временная переменная temp, 
    }                         чтобы сохранить значение cur перед обновлением prev

    return cur
} 
-}

fib :: Int -> State Context Int
fib n = do
  setVar "prev" 0
  setVar "cur" 1
  forM_' (setVar "i" 0, getVar "i" >>= \i -> return (i < n), incVar "i" 1) $ do
    prevValue <- getVar "prev"
    tempValue <- getVar "cur" -- точно
    setVar "cur" (tempValue + prevValue)
    setVar "prev" tempValue
  getVar "cur"

-------------------------------------------------------------------------------

-- 5*. State и Either (2 балла)
--     Создайте свою монаду StateWithError, обладающую сразу двумя эффектами:
--      a) эффектом монады 'State'  (изменяемое состояние);
--      b) эффектои монады 'Either' (возможность завершения вычисления ошибкой)

-- | Задайте тип
--
newtype StateWithError s e a = StateWithError (s -> Either e (a, s))


-- Реализуйте инстанс Monad для StateWithError

-- Привидите пример использования

instance Functor (StateWithError s e) where
  fmap :: (a -> b) -> StateWithError s e a -> StateWithError s e b
  fmap f (StateWithError g) = StateWithError $ \s -> case g s of
    Left e         -> Left e
    Right (a, s')  -> Right (f a, s')

instance Applicative (StateWithError s e) where
  pure :: a -> StateWithError s e a
  pure a = StateWithError $ \s -> Right (a, s)
  
  (<*>) :: StateWithError s e (a -> b) -> StateWithError s e a -> StateWithError s e b
  StateWithError mf <*> StateWithError ma = StateWithError $ \s -> case mf s of
    Left e          -> Left e
    Right (f, s')   -> case ma s' of
      Left e'       -> Left e'
      Right (a, s'') -> Right (f a, s'')

instance Monad (StateWithError s e) where
  return :: a -> StateWithError s e a
  return = pure

  (>>=) :: StateWithError s e a -> (a -> StateWithError s e b) -> StateWithError s e b
  StateWithError ma >>= f = StateWithError $ \s -> case ma s of
    Left e          -> Left e
    Right (a, s')   -> case f a of
      StateWithError mb -> mb s'

-- | Выполняет вычисление с начальным состоянием и возвращает результат или ошибку
runStateWithError :: StateWithError s e a -> s -> Either e a
runStateWithError (StateWithError f) s = case f s of
  Left e         -> Left e
  Right (a, _)   -> Right a

-- | Получает текущее состояние
getState :: StateWithError s e s
getState = StateWithError $ \s -> Right (s, s)

-- | Устанавливает новое состояние
putState :: s -> StateWithError s e ()
putState newState = StateWithError $ \_ -> Right ((), newState)

-- | Бросает ошибку
throwError :: e -> StateWithError s e a
throwError e = StateWithError $ \_ -> Left e

-- | Ловит ошибку и обрабатывает её
catchError :: StateWithError s e a -> (e -> StateWithError s e a) -> StateWithError s e a
catchError (StateWithError ma) handler = StateWithError $ \s -> case ma s of
  Left e         -> let StateWithError handler' = handler e in handler' s
  Right (a, s')  -> Right (a, s')

-- Пример использования:

-- Функция, которая делит два числа и ловит ошибку деления на ноль
divideWithCatch :: Int -> Int -> StateWithError Int String Int
divideWithCatch x y =
  catchError (do
    currentResult <- getState
    putState (currentResult + 1)  -- Модифицируем состояние для примера
    if y == 0
      then throwError "Division by zero!"
      else return (x `div` y))
  (\e -> throwError $ "Caught an error: " ++ e)


-------------------------------------------------------------------------------
