{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs  #-}

module MyLib where

import Control.Monad.State    (StateT, MonadState (..), modify)
-- import Control.Monad.Trans    (MonadTrans (..))
import Prelude hiding         (log)
import Control.Monad.Identity
-- import Control.Monad.Fail
import Control.Monad.Trans.State (get, put)
-- import Control.Monad.Trans.Class
import Control.Monad.Writer
import Control.Monad.Except
-- import Control.Monad.IO.Class
-- import System.IO (hFlush, stdout, readFile, writeFile)
-- import Text.Read (readMaybe)
-- import Data.Maybe (catMaybes)
-- import Data.List.Split (splitOn)
-- import Control.Monad (unless)
-- import Data.Map (Map)
-- import qualified Data.Map as Map

-- много закомментированных импортов из-за попыток сделать 8 задание 
-- я его сюда пока не добавил, потому что не могу заставить корректно работать как нужно, но наработки уже есть, как доделаю - дозалью 

-------------------------------------------------------------------------------

-- | Сегодня мы будем писать свой модный логгер!
--
data Logged a 
  = Logged
      { logs :: [(LoggingLevel, String)] -- накопленный лог
      , val  :: a                        -- значение
      }

-- | По уровню логирования можно понять, насколько важным
--   является увиденное сообщение. Также введение уровней логировнаия
--   позволяет фильтровать логи
--
data LoggingLevel = Debug | Info | Warning | Error 
  deriving (Eq, Show)

-- | Заведём свой трансформер на основе 'Logger'. С его помощью
--   можно будет добавить эффект логирования к эффекту любой монады.
--
newtype LoggerT m a = LoggerT { runLoggerT :: m (Logged a) } 

-- | Тип 'Logger' — просто синоним для кобминации 'LoggerT' и 'Identity',
--   которая не добавляет никакого эффекта.
--
type Logger a = LoggerT Identity a

-------------------------------------------------------------------------------

-- 1. Чтобы всё это имело смысл, сделайте 'LoggerT' монадой (1 балл)
--    При последовательных вычислениях логи должны объединяться.

instance Functor m => Functor (LoggerT m) where
  fmap :: (a -> b) -> LoggerT m a -> LoggerT m b
  fmap f (LoggerT ma) = LoggerT $ fmap (\(Logged loggs a) -> Logged loggs (f a)) ma

instance Applicative m => Applicative (LoggerT m) where
  pure :: a -> LoggerT m a
  pure a = LoggerT $ pure (Logged [] a)
  
  (<*>) :: LoggerT m (a -> b) -> LoggerT m a -> LoggerT m b
  (LoggerT mfunc) <*> (LoggerT mval) = LoggerT $ (combineLogs <$> mfunc) <*> mval
    where
      combineLogs :: Logged (a -> b) -> Logged a -> Logged b
      combineLogs (Logged logs1 f) (Logged logs2 a) = Logged (logs1 ++ logs2) (f a)

instance Monad m => Monad (LoggerT m) where
  return :: a -> LoggerT m a
  return = pure

  (>>=) :: LoggerT m a -> (a -> LoggerT m b) -> LoggerT m b
  (LoggerT ma) >>= f = LoggerT $ do
    Logged logs1 a  <- ma
    Logged logs2 b  <- runLoggerT (f a)
    return $ Logged (logs1 ++ logs2) b

-- тут hlint предлагает немного по другому записать, но мне не нравится, что он предлагает, поэтому оставлю свой вариант
instance MonadFail m => MonadFail (LoggerT m) where
  fail :: String -> LoggerT m a
  fail msg = LoggerT $ fail msg >>= return . Logged []

-------------------------------------------------------------------------------

-- | 2. Реализуйте функцию, позволяющую записать что-то в лог внутри трансформера 'LoggerT' (0,25 балла)
--
writeLog :: Monad m => LoggingLevel -> String -> LoggerT m ()
writeLog level message = LoggerT $ return (Logged [(level, message)] ())

-------------------------------------------------------------------------------

-- | 3. Реализуйте функцию `loggingModification` (0,5 балла)
--      Эта функция: 
--        1. Изменяет состояние.
--        2. Считывает его.
--        3. Если состояние удовлетворяет переданному предикату, то возвращает значение состояния.
--        4. Если не удовлетворяет, то заменяет состояние дефолтным значением и возвращает Nothing.
--
--      Про каждое из действий функция должна производить запись в лог на уровне INFO.

loggingModification :: Show s => s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification def p f = do
  -- Шаг 1: Изменяем состояние
  modify f
  lift $ writeLog Info "State modified"
  
  -- Шаг 2: Считываем изменённое состояние
  s <- Control.Monad.Trans.State.get
  lift $ writeLog Info $ "State read: " ++ show s
  
  -- Шаги 3 и 4: Проверяем состояние и возвращаем результат
  if p s
    then do
      -- Если состояние удовлетворяет предикату, возвращаем Just значение состояния
      lift $ writeLog Info $ "State passes the predicate: " ++ show s
      return (Just s)
    else do
      -- Если не удовлетворяет, заменяем состояние дефолтным и возвращаем Nothing
      Control.Monad.Trans.State.put def
      lift $ writeLog Info $ "State replaced with default: " ++ show def
      return Nothing

-------------------------------------------------------------------------------

-- | 4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'
--      и реализуйте функцию `modifyingLogging`, которая делает то же самое,
--      что и @loggingModification@ (1 балл)

instance MonadTrans LoggerT where
  lift :: Monad m => m a -> LoggerT m a
  lift m = LoggerT $ m >>= \a -> return (Logged [] a)

modifyingLogging :: (Monad m, Show s) => s -> (s -> Bool) -> (s -> s) -> LoggerT (StateT s m) ()
modifyingLogging def p f = do
  -- Шаг 1: Изменяем состояние
  lift $ modify f
  writeLog Info "State modified"

  -- Шаг 2: Считываем изменённое состояние
  s <- lift Control.Monad.Trans.State.get
  writeLog Info $ "State read: " ++ show s

  -- Шаги 3 и 4: Проверяем состояние и возвращаем результат (через логирование)
  if p s
    then do
      -- Если состояние удовлетворяет предикату, логируем соответствующее сообщение
      writeLog Info $ "State passes the predicate: " ++ show s
    else do
      -- Если не удовлетворяет, заменяем состояние дефолтным и логируем изменение
      lift $ Control.Monad.Trans.State.put def
      writeLog Info $ "State replaced with default: " ++ show def

-------------------------------------------------------------------------------

-- | 5. Сделайте так, чтобы была возможность обращаться к `LoggerT`
--      по интерфейсу `MonadState`в случае, если трансформируемая монада сама является `MonadState`.
--
--      Реализуйте функцию `modifyingLogging'`, которая делает то же самое,
--      что и @modifyingLogging@, но используя интерфейс 'MonadTrans'. (1 балл)
--

instance MonadState s m => MonadState s (LoggerT m) where
  get = lift Control.Monad.State.get
  put = lift . Control.Monad.State.put

modifyingLogging' :: (MonadState s m, Show s, Monad m) => s -> (s -> Bool) -> (s -> s) -> LoggerT m ()
modifyingLogging' def p f = do
  -- Шаг 1: Изменяем состояние
  modify f
  writeLog Info "State modified"

  -- Шаг 2: Считываем изменённое состояние
  s <- Control.Monad.State.get
  writeLog Info $ "State read: " ++ show s

  -- Шаги 3 и 4: Проверяем состояние и возвращаем результат (через логирование)
  if p s
    then do
      -- Если состояние удовлетворяет предикату, логируем соответствующее сообщение
      writeLog Info $ "State passes the predicate: " ++ show s
    else do
      -- Если не удовлетворяет, заменяем состояние дефолтным и логируем изменение
      Control.Monad.State.put def
      writeLog Info $ "State replaced with default: " ++ show def

-------------------------------------------------------------------------------

-- | 6. Сделайте 'LoggerT' представителем класса типов 'MonadLogger',
--      позволяющем производить запись в лог в произвольном представителе.
--
--      Сделайте любой трансформер с 'LoggerT' внутри представителем класса типов 'MonadLogger'.
--
--      Реализуйте функцию `loggingModification'`, которая делает то же самое,
--      что и `loggingModification`, но использует интерфейс `MonadLogger`. (1,25 балла)
--

class MonadLogger m where
  log :: LoggingLevel -> String -> m ()

instance Monad m => MonadLogger (LoggerT m) where
  log level message = LoggerT $ return (Logged [(level, message)] ())

instance (MonadTrans t, Monad m, MonadLogger m) => MonadLogger (t m) where
  log level message = lift $ log level message

loggingModification' :: (MonadState s m, MonadLogger m, Show s) => s -> (s -> Bool) -> (s -> s) -> m (Maybe s)
loggingModification' def p f = do
  -- Шаг 1: Изменяем состояние
  modify f
  log Info "State modified"
  
  -- Шаг 2: Считываем изменённое состояние
  s <- Control.Monad.State.get
  log Info $ "State read: " ++ show s
  
  -- Шаги 3 и 4: Проверяем состояние и возвращаем результат
  if p s
    then do
      -- Если состояние удовлетворяет предикату, логируем это
      log Info $ "State passes the predicate: " ++ show s
      return (Just s)
    else do
      -- Если не удовлетворяет, заменяем состояние дефолтным
      Control.Monad.State.put def
      log Info $ "State replaced with default: " ++ show def
      return Nothing

-------------------------------------------------------------------------------

-- | 7. Создайте из монадических трансформеров монаду, в которой можно:
--        1. Производить запись в лог.
--        2. Хранить состояние.
--        3. Завершаться с ошибкой (смотри Control.Monad.Except).
--        4. Производить IO-вычисления. 
--
--      Помните, что порядок, в котором вы заворачиваете друг в друга монады, важен. (1,5 балла)
--
type LoggingStateWithErrorInIO stateType errorType resType =
  ExceptT errorType (StateT stateType (LoggerT IO)) resType

-------------------------------------------------------------------------------

-- | 8. Реализуйте функцию-программу `processUserDB`, которая:
--        1. Сначала считывает из файла базу пользователей. 
--           Список пользователей в процессе исполнения программы должен храниться в стейте.
--           После считывания файла должна появиться соответствующая запись в логе.
--        2. Считывает из stdin команды вида "GIVE ACCES <user_id> <access_rights>".
--           Каждая команда назначает пользователю <user_id> заданные права <access_rights>.
--           О назначении прав конкретному пользователю должна появляться запись в логе.
--        3. Если в stdin приходит команда "STOP", то считывание команд прекращается.
--           Действите тоже долно быть залогировано.
--        4. После окончания считывания команд должна произойти запись в файлы:
--             - лога программы,
--             - пользователей с назначенными им правами.
--
--     Если какое-то действие совершить не удаётся, то должна быть выброшена не IO-шная ошибка
--     и произведена запись в лог. При этом ошибка должна обрывать исполнение программы.
--     Важно, что при завершении программы из-за ошибки должна произойти запись лога в файл.
--
--     (3,5 балла)
--      

-- -- | Тип пользовательского id.
-- --
-- type UserId = Int

-- -- | Тип возможных прав пользователей.
-- --
-- data AccessRights = Read | Write | ReadAndWrite | Admin deriving (Show, Read, Enum, Eq)

-- -- предположим, что пользователи представлены как Map UserId AccessRights
-- type UserDB = Map.Map UserId AccessRights

-- -- Состояние включает в себя базу пользователей
-- data YourState = YourState { userDB :: UserDB }
-- data YourError = FileNotFoundError | ParseError | UserNotFoundError UserId | UnknownCommandError String | StopCommandReceived

-------------------------------------------------------------------------------
