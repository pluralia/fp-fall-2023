-- по согласованию 1 и 2 из 9, 1 из 10
-- и я не поняла, как писать тесты здесь

-- cabal поругался на определенные но не использованные переменные из Writer и на не завершенный resolve

module MyLib where

import           Data.List
import           Data.Maybe
import           Control.Monad.Writer.Lazy
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
-- import qualified System.Random as R -- cabal install --lib  random

-------------------------------------------------------------------------------
-- HW9
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
-- для снижения ленивости по совету hlint
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

-- | Возвращает первое правило, действующее на пакет
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#v:find
match :: [Rule] -> Packet -> Maybe Rule
match rules packet = find (\r -> source r == pSource packet && destination r == pDestination packet) rules

-- | Фильтрует 1 пакет
--   В логе должны быть сообщения 2 видов:
--      1) Нет правила для пакета (указать пакет)
--      2) Есть правило для пакета (указать пакет и правило)
-- 
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = 
    case match rules packet of
        Nothing -> logMsg ("No rule for packet, reject " <> show packet) 
                >> return Nothing
        Just rule -> logMsg (show packet <> "accepted by rule " <> show rule)
                  >> return (if action rule == Accept then Just packet else Nothing)

-- Немного усложним задачу: теперь мы хотим объединить дублирующиеся последовательные записи в журнале.
-- Ни одна из существующих функций не позволяет изменять результаты предыдущих этапов вычислений,
-- но мы можем использовать прием "отложенного протоколирования", чтобы добавлять запись в лог только
-- после получения новой записи, которая не совпадает с предыдущими.

-- | Объединяет одинаковые записи в конце лога
  -- Эта функция использует [Entry] и в качестве типа лога, и в качестве типа результата.
  --   - При объединении двух одинаковых сообщений результатом будет только сообщение с увеличенным счетчиком,
  --     при этом в лог ничего не добавляется
  --   - При объединении двух разных сообщений, первое записывается в лог, а второе возвращается в качестве результата
  --     (мы запишем второе сообщение в лог, когда-нибудь потом -- когда придет новое сообщение, отличное от него --
  --     в этом и есть суть "отложенного протоколирования")


-- я не поняла, как дальше это задание делать
mergeEntries :: [Entry] -> [Entry] -> Writer [Entry] [Entry]
mergeEntries = undefined

-- | Применяет функцию fn к каждому значению из списка xs, чтобы получить список Writer.
--   Затем запускает каждый Writer и объединяет логи c помощью merge, используя initial в качестве начального лога.
--   Результатом работы функции является Writer, значение которого -- список всех значений из всех Writer,
--   а лог -- результат слияния всех логов всех Writer.
--
-- | Для нашего брандмауэра:
--     1) xs -- список пакетов
--     2) fn -- функция, производящая Writer из пакета
--     3) initial -- начальный лог
--     4) merge -- функция, которая мержит 2 лога в один и производит из этого Writer
--
groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial merge xs fn = undefined

-- | Фильтрует список пакетов и возвращает список отфильтрованных пакетов и логи
--   В начале и конце обработки пакетов добавьте сообщения о начале и конце логирования
--   Используйте groupSame
--
filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll = undefined

-------------------------------------------------------------------------------

-- 2. Reader: Инстанцирование шаблонов (2 балла)

-- Рассмотрим проблему инстанцирования шаблонов, содержащих подстановки переменных и включенные шаблоны.

-- Пример проблемы из TypeScript:
--   const y = 32;
--   const x = `I want "${y}" apples.`;
-- ожидаем, получить в x строку "I want 32 apples."

-- Используя монаду Reader, мы можем поддерживать окружение/контекст всех известных шаблонов и всех известных переменных.
-- Когда встречается подстановка переменной, мы можем использовать функцию asks для поиска значения переменной в окружении.
-- Если в шаблон включены определения новых переменных, мы будем должны изменить окружение, добавив в него эти перменные;
-- продолжить работу на измененном окружении можно с помощью функции local

-- | Зададим тип шаблона
--
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
-- поиск по ключу в окружении
lookupVar :: String -> Environment -> Maybe String
lookupVar name env = M.lookup name (vars env)

-- | Ищет шаблон в окружении
-- поиск по ключу в окружении
lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name env = M.lookup name (templs env)

-- | Добавляет новые переменные в окружение
-- 
addDefs :: M.Map String String -> Environment -> Environment
addDefs newVars env = env {vars = newVars `M.union` vars env}

-- | Резолвит (подставляет все неизвестные) определение
--   и выдает в качестве ответа пару (имя переменной, значение)
--
-- это я поняла с Вашего объяснения на лекции: мы просто используем определенную функцию в которой прописан паттерн-матчиг 
-- и возвращаем пару (имя-значение)
resolveDef :: Definition -> Reader Environment (String, String)
resolveDef (Definition t1 t2) = do
    name <- resolve t1
    value <- resolve t2
    return (name, value)

-- | Резолвит (подставляет все неизвестные) шаблон в строку
--     Пример использования:
--     ts = Compound [Text "I want ", Var . Text $ "y", Text " apples."]
--     mapM resolve ts ->  ["I want ", "32", " apples."]
--
-- Подсказки:
-- 1) Quote -- штука, аналогичная Var, но для шаблонов -- нужно зарезолвить входящий шаблон до имени, а потом поискать его в environment
-- 2) Include -- здесь необходимо зарезолвить шаблон templ с учетом определений defs. Для этого необходимо:
--    a. Получить имя шаблона через resolve templ
--    b. Найти тело шаблона по его имени в environment
--    с. Если тела не нашлось, то вернуть "", а если нашлось -- зарезолвить defs, обновить ими environment, и на новом environment зарезолвить тело
-- 3) Compound -- здесь могут пригодиться `traverse` и `mconcat`
--

-- из объяснения я поняла, что здесь работает паттерн-матчинг и рекурсия,
-- но не поняла, как это обрабатывается для Compound и Include
resolve :: Template -> Reader Environment String
resolve (Text text) = return text

resolve (Var var) = do
    name <- resolve var
    value <- asks (lookupVar name)
    return (fromMaybe "" value)

resolve (Quote quote) = do
    name <- resolve quote
    value <- asks (lookupTemplate name)
    return (maybe "" show value)

{-
resolve (Compound compound) = do 
    name <- resolve include
    value <- ????

resolve (Include include) = do
    name <- resolve include
    value <- ????
-}

-- | Пример использования
--
test :: String
test = runReader (resolve template) $ Env M.empty (M.fromList [("y", "32")])
  where
    template :: Template
    template = Compound [Text "I want ", Var . Text $ "y", Text " apples."]

-------------------------------------------------------------------------------
-- HW 10
-- 1. State (1,5 балла)

-- В этом задании мы реализуем конечный автомат, моделирующий турникет с монетоприемником.
-- При этом наша модель будет вести лог: на каждый вход (Coin, Push) она будет давать вывод (Thank, Open или Tut)

-- | Турникет имеет два состояния: Locked и Unlocked (он начинает работу в состоянии Locked)
-- 
data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

-- | И 2 возможных действия (входа)
--   1. Coin (кто-то опускает монету)
--   2. Push (кто-то толкает рычаг)
data TurnstileInput = Coin | Push
  deriving (Eq, Show)

-- Каждое действие приводит к выводу (Thank, Open или Tut) и переходу в новое или то же самое состояние.
-- Схема: https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State#/media/File:Turnstile_finite-state_machine_with_actions.svg
--   1. Если опускают монету в заблокированный турникет, то нужно поблагодарить, и турникет разблокируется
--   2. Если монету опускают в разблокированный турникет, то нужно поблагодарить, но состояние турникета не изменяется
--   3. Если толкнуть рычаг, когда турникет разблокирован, рычаг откроется, чтобы пропустить его, а затем закроется, чтобы не пропустить никого другого
--   4. Если толкнуть рычаг, когда турникет заблокирован, он вежливо погладит его, но не пропустит и останется запертым.

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

-- обработка возможных входов
turnstileSwitch :: TurnstileState -> TurnstileInput -> (TurnstileOutput, TurnstileState)
turnstileSwitch _        Coin = (Thank, Unlocked)
turnstileSwitch Unlocked Push = (Open, Locked)
turnstileSwitch Locked   Push = (Tut, Locked)

-- | Реализуйте автомат

-- действия [Coin, Coin, Push, Push, Coin]
turnstileActs :: [TurnstileInput]
turnstileActs = [Coin, Coin, Push, Push, Coin]
--
-- finite state machine
-- типы: переходы -> начальное состояние -> Состояние + Выход
fsm :: (TurnstileState -> TurnstileInput -> (TurnstileOutput, TurnstileState)) 
        -> TurnstileInput -> State TurnstileState TurnstileOutput
fsm trans inputAction = state $ \s -> trans s inputAction

turnstile :: State TurnstileState [TurnstileOutput]
turnstile = mapM (fsm turnstileSwitch) turnstileActs

-- Привидите пример запуска на последовательности действий [Coin, Coin, Push, Push, Coin]
-- (можете привести свою любой длины и содержания)

-------------------------------------------------------------------------------