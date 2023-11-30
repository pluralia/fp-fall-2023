{-# LANGUAGE InstanceSigs #-}
module MyLib where

import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import qualified System.Random as R -- cabal install --lib  random
import           Data.List
import           Data.Maybe
import           Control.Monad
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
data IP = IP Int Int Int Int
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

-- | Возвращает первое правило, действующее на пакет
--
match :: [Rule] -> Packet -> Maybe Rule
match rules packet = find ruleMatches rules
  where
    ruleMatches rule = source rule == pSource packet && destination rule == pDestination packet

-- | Фильтрует 1 пакет
-- 
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet =
    case match rules packet of
        Nothing -> logMsg "No matching rule, packet rejected" >> return Nothing
        Just rule -> logMsg (show rule)
                  >> return (if action rule == Accept then Just packet else Nothing)

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
mergeEntries xs [] = return xs
mergeEntries [] (y:ys) = mergeEntries [y] ys
mergeEntries (x:xs) (y:ys)
    | msg x == msg y = let newX = x {count = count x + count y} in mergeEntries (newX:xs) ys
    | otherwise = do
        tell (x:xs)
        mergeEntries [y] ys

-- | Применяет входную функцию к списку значений, чтобы получить список Writer.
--   Затем запускает каждый Writer и объединяет результаты.
--   Результатом работы функции является Writer, значение которого -- список всех значений из всех Writer,
--   а лог -- результат слияния всех логов всех Writer
--
-- 'initial' -- изначальное значение лога
--
groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial merge xs fn = go initial xs
  where
    go elem' [] = tell elem' >> return []
    go elem' (x:xs') = 
      let (value, log') = runWriter (fn x)
          acc = fst $ runWriter $ merge elem' log'
      in do
        tell acc
        values <- go acc xs'
        return (value:values)

-- | Фильтрует список пакетов и возвращает список отфильтрованных пакетов и логи
--
filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll rules = fmap catMaybes . mapM (filterOne rules) 

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
lookupTemplate tmpl env = M.lookup tmpl (templs env)

-- | Добавляет новые переменные в окружение
-- 
addDefs :: M.Map String String -> Environment -> Environment
addDefs newVars env = env {vars = M.union (vars env) newVars}

-- | Резолвит (подставляет все неизвестные) шаблон и выдает в качестве ответа
--   пару (имя шаборна, значение)
--
resolveDef :: Definition -> Reader Environment (String, String)
resolveDef (Definition name _) = do
  env <- ask
  case lookupTemplate (getName name) env of
    Just tmpl -> return (getName name, show tmpl)
    Nothing -> return (getName name, "Template not found")

getName :: Template -> String
getName (Var (Text s)) = s
getName (Text s) = s
getName (Quote t) = "Quote: " ++ getName t
getName (Include t _) = "Include: " ++ getName t
getName (Compound ts) = "Compound: " ++ unwords (map getName ts)
getName _ = "default"

-- | Резолвит (подставляет все неизвестные) шаблон в строку
--
resolve :: Template -> Reader Environment String
resolve (Text str) = return str

resolve (Var name) = do
  env <- ask
  case lookupVar (getName name) env of
    Just var -> return var
    Nothing -> return ""

resolve (Quote template) = resolve template

resolve (Include template defs) = do
  newEnv <- asks createNewEnv
  local (const newEnv) (resolve template)
  where
    createNewEnv oldEnv = foldl addDefToEnv oldEnv defs
    addDefToEnv env (Definition t v) = addDefs (M.singleton (show t) (show v)) env

resolve (Compound templates) = do
  resolvedTemplates <- mapM resolve templates
  return (concat resolvedTemplates)

-------------------------------------------------------------------------------

-- 3. State: Генерация случайного значения кастомного типа (1 балл)

-- Чистый функциональный язык не может обновлять значения на месте (почему?) 
-- потому что он вообще не может менять никакие состояния, иначе бы были сайд эффекты
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
-- получается, мы нарушаем правило про чистоту функций - что она должна всегда возвращть одно и то же значение при одинаковых входных данных
-- Монада State скрывает потоковую передачу состояния (как вы понимаете слова "потоковая передача состояния") внутри операции >>=,
-- делая код проще для написания, чтения и модификации.
-- потоковая передача состояния - мы передаем состояние от одной функции к другой


-- | Возвращает случайное значение и обновляет состояние генератора случайных чисел
--
getAny' :: (R.Random a) => State R.StdGen a
getAny' = do
    gen <- get
    let (value, newGen) = R.random gen
    put newGen
    return value

-- | Аналогична getAny, но генерирует значение в границах
--
getOne :: (R.Random a) => (a, a) -> State R.StdGen a
getOne bounds = do
    gen <- get
    let (value, newGen) = R.randomR bounds gen
    put newGen
    return value

-- | Используя монаду State с StdGen в качестве состояния, мы можем генерировать случаные значения
--   заданного типа, не передавая состояния генератора случайных чисел в коде вручную
--
makeRandomValueST :: R.StdGen -> (MyType, R.StdGen)
makeRandomValueST = runState $ do
    n <- getOne (1, 100)
    b <- getAny'
    c <- getOne ('a', 'z')
    m <- getOne (-n, n)
    return (MT n b c m)

-------------------------------------------------------------------------------

-- 4. "Императивное" программирование (2 балла)

---------------------------------------

-- | 4.1. Реализуйте функцию `whileM_`, исполняющую заданное вычисление,
--        пока первое вычисление возвращает 'True'
--
whileM_ :: (Applicative m, Monad m) => m Bool -> m a -> m ()
whileM_ mb ma = do
    b <- mb
    Control.Monad.when b $ ma >> whileM_ mb ma -- это hlint предложил, но мне кажется с If понятнее
    -- if b
    --     then ma >> whileM_ mb ma
    --     else return ()

---------------------------------------

-- | 4.2. Реализуйте функцию `forM_`, являющуюуся аналогом цикла for.
--        Перед входом в цикл вычисляется init, в итерациях цикла вычисляется body, 
--        в конце каждой итерации цикла вычисляется nextIter, итерации идут, 
--        пока выполняется условие cond
--
forM_ :: Monad m => (m (), m Bool, m ()) -> m a -> m ()
forM_ (init', cond, nextIter) body = do
    init'
    loop
  where
    loop = do
        c <- cond -- опять предложил hlint, тут вроде одинаково понятно в обоих случаях
        when c $ do 
            _ <- body
            nextIter
            loop
        -- if c
        --     then do
        --         _ <- body
        --         nextIter
        --         loop
        --     else return ()

---------------------------------------

-- | Окружение / контекст: имя переменной в значение
--
type Context = M.Map String Int

-- 4.3. Реализуйте следующие функции

-- | Задаёт значение переменной. 
--   Если переменная есть в контексте, перезаписывает её значение.
--
setVar :: String -> Int -> State Context ()
setVar var value = do
    context <- get
    put (M.insert var value context)

-- | Увеличивает значение переменной. 
--   Если её нет в контексте, то кидает ошибку.
--
incVar :: String -> Int -> State Context ()
incVar var inc = do
    context <- get
    case M.lookup var context of
        Just value -> put (M.insert var (value + inc) context)
        Nothing -> error "Variable not found in context"

-- | Достаёт из контекста значение заданной переменной.
--   Если переменной нет в контексте, то кидает ошибку.
--
getVar :: String -> State Context Int
getVar var = do
    context <- get
    case M.lookup var context of
        Just value -> return value
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

fib :: Int -> State Context Int
fib 0 = do
    do M.findWithDefault 0 "prev" <$> get
fib n = do
    context <- get
    let prev = M.findWithDefault 0 "prev" context
    let cur = M.findWithDefault 0 "cur" context
    put $ M.fromList [("prev", cur), ("cur", prev + cur)]
    fib (n - 1)

-------------------------------------------------------------------------------

-- 5*. State и Either (2 балла)
--     Создайте свою монаду StateWithError, обладающую сразу двумя эффектами:
--      a) эффектом монады 'State'  (изменяемое состояние);
--      b) эффектои монады 'Either' (возможность завершения вычисления ошибкой)

-- | Задайте тип
--
newtype StateWithError s e a
  = StateWithError {runStateWithError :: s -> Either e (s, a)}

instance Functor (StateWithError s e) where
    fmap :: (a -> b) -> StateWithError s e a -> StateWithError s e b
    fmap f m = StateWithError $ \s1 -> case runStateWithError m s1 of
        Left e      -> Left e
        Right (s2,a) -> Right (s2, f a)

instance Applicative (StateWithError s e) where
    pure :: a -> StateWithError s e a
    pure a = StateWithError $ \s -> Right (s, a)
    
    (<*>) :: StateWithError s e (a -> b) -> StateWithError s e a -> StateWithError s e b
    mf <*> ma = StateWithError $ \s1 -> case runStateWithError mf s1 of
        Left e      -> Left e
        Right (s2,f) -> case runStateWithError ma s2 of
            Left e      -> Left e
            Right (s3,a) -> Right (s3, f a)

-- instance Monad (StateWithError s e) where
--     return :: a -> StateWithError s e a
--     return = pure

    --(>>=) :: StateWithError s e a-> (a -> StateWithError s e b) -> StateWithError s e b

-- Реализуйте инстанс Monad для StateWithError

-- Привидите пример использования

-------------------------------------------------------------------------------
