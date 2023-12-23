{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

import Control.Monad.State    (State, StateT, MonadState (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Trans    (MonadTrans (..))
import Prelude hiding         (log)

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
  fmap  = undefined

instance Applicative m => Applicative (LoggerT m) where
  pure  = undefined
  (<*>) = undefined

instance Monad m => Monad (LoggerT m) where
  (>>=)  = undefined

instance MonadFail m => MonadFail (LoggerT m) where
  fail = undefined

-------------------------------------------------------------------------------

-- | 2. Реализуйте функцию, позволяющую записать что-то в лог внутри трансформера 'LoggerT' (0,25 балла)
--
wrtiteLog :: Monad m => LoggingLevel -> String -> LoggerT m ()
wrtiteLog = undefined

-------------------------------------------------------------------------------

-- | 3. Реализуйте функцию `loggingModification` (0,5 балла)
--      Эта функция: 
--        1. Изменяет состояние.
--        2. Считывает его.
--        3. Если состояние удовлетворяет переданному предикату, то возвращает значение состояния.
--        4. Если не удовлетворяет, то заменяет состояние дефолтным значением и возвращает Nothing.
--
--      Про каждое из действий функция должна производить запись в лог на уровне INFO.

loggingModification :: s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification def p f = undefined

-------------------------------------------------------------------------------

-- | 4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'
--      и реализуйте функцию `modifyingLogging`, которая делает то же самое,
--      что и @loggingModification@ (1 балл)

instance MonadTrans LoggerT where
  lift = undefined

modifyingLogging :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) (Maybe s)
modifyingLogging def p f = undefined

-------------------------------------------------------------------------------

-- | 5. Сделайте так, чтобы была возможность обращаться к `LoggerT`
--      по интерфейсу `MonadState`в случае, если трансформируемая монада сама является `MonadState`.
--
--      Реализуйте функцию `modifyingLogging'`, которая делает то же самое,
--      что и @modifyingLogging@, но используя интерфейс 'MonadTrans'. (1 балл)
--

instance MonadState s m => MonadState s (LoggerT m) where
  put = undefined
  get = undefined

modifyingLogging' :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) (Maybe s)
modifyingLogging' def p f = undefined

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
  log = undefined

instance (MonadTrans t, Monad m) => MonadLogger (t (LoggerT m)) where
  log = undefined

loggingModification' :: s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification' def p f = undefined

-------------------------------------------------------------------------------

-- | 7. Создайте из монадических трансформеров монаду, в которой можно:
--        1. Производить запись в лог.
--        2. Хранить состояние.
--        3. Завершаться с ошибкой (смотри Control.Monad.Except).
--        4. Производить IO-вычисления. 
--
--      Помните, что порядок, в котором вы заворачиваете друг в друга монады, важен. (1,5 балла)
--
type LoggingStateWithErrorInIO stateType errorType resType = ()

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

-- | Тип пользовательского id.
--
type UserId = Int

-- | Тип возможных прав пользователей.
--
data AccessRights = Read | Write | ReadAndWrite | Admin

data YourState = YourState
data YourError = YourError

processUserDB :: FilePath -> FilePath -> FilePath -> LoggingStateWithErrorInIO YourState YourError ()
processUserDB pathToInputDB pathToLog pathToOutDB = undefined

-------------------------------------------------------------------------------