module MyLib where

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Control.Monad (when, foldM)
import           Data.List (find)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified System.Random as R -- cabal install --lib  random
-- import           Data.Functor.Identity

-- Не написаны функции groupSame (задание 1) и fib (задание 4.4)

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
newtype IP = IPAddress String
  deriving (Show, Eq)

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
    } deriving (Show, Eq)

-- Функция для проверки соответствия пакета правилу
matchesRule :: Packet -> Rule -> Bool
matchesRule packet rule =
  pSource packet == source rule && pDestination packet == destination rule

-- | Возвращает первое правило, действующее на пакет
--
match :: [Rule] -> Packet -> Maybe Rule
match rules packet = find (matchesRule packet) rules

-- Вспомогательная функция для вывода сообщения о пакете
logPacket :: String -> Packet -> String
logPacket actionMsg packet =
  "Packet from " ++ show (pSource packet) ++ " to " ++ show (pDestination packet) ++ " " ++ actionMsg

-- | Фильтрует 1 пакет
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do
  let maybeMatchedRule = match rules packet -- Находим первое правило, действующее на пакет
  case maybeMatchedRule of
    Just matchedRule -> do
      if action matchedRule == Accept
        then do
          logMsg $ logPacket "was accepted" packet -- Используем вспомогательную функцию для вывода сообщения о принятом пакете
          return (Just packet) -- Если правило указывает на аксепт, то возвращаем пакет
        else do
          logMsg $ logPacket "was rejected" packet -- Используем вспомогательную функцию для вывода сообщения об отклоненном пакете
          return Nothing -- Если правило указывает на реджект, то возвращаем Nothing
    Nothing -> do
      logMsg $ logPacket "has no matching rule" packet -- Используем вспомогательную функцию для вывода сообщения о пакете без соответствующего правила
      return Nothing -- Если нет соответствующего правила, то возвращаем Nothing

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
mergeEntries [] ys = tell ys >> return []
mergeEntries xs [] = tell xs >> return []
mergeEntries (x:xs) (y:ys)
  | msg x == msg y = do 
    let merged = Log (count x + count y) (msg x)
    mergeEntries [merged] ys
    
  | otherwise = do
    tell [x]
    mergeEntries xs (y : ys)

-- | Применяет входную функцию к списку значений, чтобы получить список Writer.
--   Затем запускает каждый Writer и объединяет результаты.
--   Результатом работы функции является Writer, значение которого -- список всех значений из всех Writer,
--   а лог -- результат слияния всех логов всех Writer
--
-- 'initial' -- изначальное значение лога

groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial mergeFun values applyFun = do
  -- Применим applyFun к каждому элементу списка values и создадим список промежуточных Writer (с их логами)
  let writers = fmap applyFun values

  -- Выполним операцию runWriter для каждого Writer, чтобы получить их значения и логи
  let (results, logs) = unzip $ fmap runWriter writers

  -- Пройдем по списку логов и объединим их с использованием функции mergeFun
  finalLog <- foldM (\accLog curLog -> do mergeFun accLog curLog
                     ) initial logs

  -- Создаем Writer, в котором лог - объединение всех логов, а значение - список значений всех Writer
  writer (results, finalLog)

-- | Фильтрует список пакетов и возвращает список отфильтрованных пакетов и логи
--
filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll rules packets = do
  -- Применяем функцию filterOne ко всем пакетам, собираем логи
  filteredPackets <- groupSame [] mergeEntries packets (filterOne rules)
  -- Возвращаем только отфильтрованные пакеты
  return (catMaybes filteredPackets)

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
lookupVar varName env = M.lookup varName (vars env)

-- | Ищет шаблон в окружении
--
lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate templateName env = M.lookup templateName (templs env)

-- | Добавляет новые переменные в окружение
-- 
addDefs :: M.Map String String -> Environment -> Environment
addDefs newVars env = env { vars = M.union newVars (vars env) }

-- | Резолвит (подставляет все неизвестные) шаблон и выдает в качестве ответа
--   пару (имя шаблона, значение)
-- не уверен, что правильно понял задание (мне бы 1 пример увидеть) и поэтому не уверен, что правильно написал
resolveDef :: Definition -> Reader Environment (String, String)
resolveDef (Definition templateName value) = do
  env <- ask
  let resolvedValue = runReader (resolveTemplate value) env
  return (getTemplateName templateName, resolvedValue)

resolveTemplate :: Template -> Reader Environment String
resolveTemplate (Text s) = return s
resolveTemplate (Var t) = do
  env <- ask
  case t of
    Text name -> do
      let maybeValue = lookupVar name env
      case maybeValue of
        Just value -> return value
        Nothing -> return ""
    _ -> resolveTemplate t
resolveTemplate _ = return ""

getTemplateName :: Template -> String
getTemplateName (Text name) = name
getTemplateName (Var template) = getTemplateName template
getTemplateName _ = ""

-- | Резолвит (подставляет все неизвестные) шаблон в строку
--
resolve :: Template -> Reader Environment String
resolve = resolveTemplate

-------------------------------------------------------------------------------

-- 3. State: Генерация случайного значения кастомного типа (1 балл)

-- Чистый функциональный язык не может обновлять значения на месте (почему?)

-- Потому что суть функционального языка - это функции. И они должны быть чистыми
-- То есть давать одинаковый результат при одних и тех же входных данных и не иметь побочных эффектов
-- Обновление значений на месте, как это делается, например, в императивных языках программирования, 
-- может привести к изменению состояния, которое не соответствует принципу неизменяемости данных в функциональном программировании

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

-- С таким подходом может получится очень много параметров в функции, что сделает код нечитаемым и трудным для изменения
-- потому что вручную настраивать каждый конкретный параметр это тяжко, а если их не 1-2-3, а 10-20-30, то совсем больно 

-- Монада State скрывает потоковую передачу состояния (как вы понимаете слова "потоковая передача состояния") внутри операции >>=,
-- делая код проще для написания, чтения и модификации.

-- она позволяет передавать и изменять состояние внутри вычислений, избегая явного передачи параметров состояния через каждую функцию

-- | Возвращает случайное значение и обновляет состояние генератора случайных чисел
--
getAny :: (R.Random a) => State R.StdGen a
getAny = state R.random

-- | Аналогична getAny, но генерирует значение в границах
--
getOne :: (R.Random a) => (a, a) -> State R.StdGen a
getOne bounds = state $ R.randomR bounds

-- | Используя монаду State с StdGen в качестве состояния, мы можем генерировать случаные значения
--   заданного типа, не передавая состояния генератора случайных чисел в коде вручную
--
makeRandomValueST :: R.StdGen -> (MyType, R.StdGen)
makeRandomValueST = runState $ do
    n <- getOne (1, 100)
    b <- getAny
    c <- getOne ('a', 'z')
    m <- getOne (-n, n)
    return $ MT n b c m

-------------------------------------------------------------------------------

-- 4. "Императивное" программирование (2 балла)

---------------------------------------

-- | 4.1. Реализуйте функцию `whileM_`, исполняющую заданное вычисление,
--        пока первое вычисление возвращает 'True'
-- тут Hlint предлагает заменить then на when, я попробовал, но тогда у меня ничего не работает
-- почему-то мой компилятор when не распознает, поэтому оставил так 
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ condd actionn = do
  check <- condd
  when check
    $ do _ <- actionn
         whileM_ condd actionn

---------------------------------------

-- | 4.2. Реализуйте функцию `forM_`, являющуюуся аналогом цикла for.
--        Перед входом в цикл вычисляется init, в итерациях цикла вычисляется body, 
--        в конце каждой итерации цикла вычисляется nextIter, итерации идут, 
--        пока выполняется условие cond
--
-- тут Hlint предлагает заменить then на when, я попробовал, но тогда у меня ничего не работает
-- почему-то мой компилятор when не распознает, поэтому оставил так 
forM_ :: Monad m => (m (), m Bool, m ()) -> m a -> m ()
forM_ (initt, cond, nextIter) body = do
  initt
  check <- cond
  when check
    $ do _ <- body
         nextIter
         MyLib.forM_ (initt, cond, nextIter) body

---------------------------------------

-- | Окружение / контекст: имя переменной в значение
--
type Context = M.Map String Int

-- 4.3. Реализуйте следующие функции

-- | Задаёт значение переменной. 
--   Если переменная есть в контексте, перезаписывает её значение.
--
setVar :: String -> Int -> State Context ()
setVar varName varValue = do
  context <- get
  let updatedContext = M.insert varName varValue context
  put updatedContext

-- | Увеличивает значение переменной. 
--   Если её нет в контексте, то кидает ошибку.
--
incVar :: String -> Int -> State Context ()
incVar varName amount = do
  context <- get
  case M.lookup varName context of
    Just val -> do
      let updatedContext = M.insert varName (val + amount) context
      put updatedContext
    Nothing -> error "Variable not found in context"

-- | Достаёт из контекста значение заданной переменной.
--   Если переменной нет в контексте, то кидает ошибку.
--
getVar :: String -> State Context Int
getVar varName = do
  context <- get
  case M.lookup varName context of
    Just val -> return val
    Nothing -> error "Variable not found in context"

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

-- fib :: Int -> State Context Int
-- fib n = do
--   setVar "prev" 0
--   setVar "cur" 1
--   forM_ (1, (checkLessThanOrEqual n), plusOneInState) $ do
--     prev <- getVar "prev"
--     cur <- getVar "cur"
--     let c = cur
--     incVar "cur" (prev + cur)
--     incVar "prev" c
--   getVar "cur"

-- -- Оборачиваем функцию <= n в монадический контекст
-- checkLessThanOrEqual :: Int -> StateT Context Identity Bool
-- checkLessThanOrEqual n = do
--   currentState <- get
--   let result = M.size currentState <= n
--   return result

-- plusOneInState :: StateT Context Identity ()
-- plusOneInState = state $ \currentState -> ((), M.map (+1) currentState)

-------------------------------------------------------------------------------

-- 5*. State и Either (2 балла)
--     Создайте свою монаду StateWithError, обладающую сразу двумя эффектами:
--      a) эффектом монады 'State'  (изменяемое состояние);
--      b) эффектои монады 'Either' (возможность завершения вычисления ошибкой)

-- | Задайте тип
newtype StateWithError str err a = StateWithError (str -> Either err (a, str))

-- Реализуйте инстанс Monad для StateWithError

instance Functor (StateWithError str err) where
  fmap f (StateWithError g) = StateWithError $ \s -> case g s of
    Left err -> Left err
    Right (a, s') -> Right (f a, s')

instance Applicative (StateWithError str err) where
  pure a = StateWithError $ \s -> Right (a, s)
  StateWithError mf <*> StateWithError mx = StateWithError $ \s -> case mf s of
    Left err -> Left err
    Right (f, s') -> case mx s' of
      Left err -> Left err
      Right (x, s'') -> Right (f x, s'')

instance Monad (StateWithError str err) where
  return = pure
  StateWithError mx >>= f = StateWithError $ \s -> case mx s of
    Left err -> Left err
    Right (x, s') -> let StateWithError g = f x in g s'

-- Привидите пример использования

-- Функция для изменения состояния
modifyState :: str -> StateWithError str err ()
modifyState newState = StateWithError $ \_ -> Right ((), newState)

-- Функция, выдающая текущее состояние
getState :: StateWithError str err str
getState = StateWithError $ \s -> Right (s, s)

-- Функция, которая могла бы провести вычисления, способные завершиться ошибкой
performCalculation :: Int -> StateWithError String String Int
performCalculation x = do
  sstate <- getState
  when (length sstate < 5) $ modifyState "Error: State length too short"
  return (x * 2)

-- В примере performCalculation пытается выполнить вычисление (изменяемое состояние), завершающееся ошибкой, 
-- если текущее состояние не удовлетворяет определенному условию (возможность завершения вычисления ошибкой)
-------------------------------------------------------------------------------
