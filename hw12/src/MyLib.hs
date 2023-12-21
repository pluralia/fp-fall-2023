{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs     #-}

module MyLib where

{- cabal:
    build-depends: base, mtl, containers
-}
{- install for ghci:
    > cabal install mtl
    > ghci
    >> :set -package mtl
-}

import Control.Monad.State    
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans    ()
import Prelude hiding         (log)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Except (ExceptT)

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
  fmap f (LoggerT ma) = LoggerT $ fmap (\(Logged logs' a) -> Logged logs' (f a)) ma

instance Applicative m => Applicative (LoggerT m) where
  pure a = LoggerT $ pure (Logged [] a)
  (LoggerT mf) <*> (LoggerT ma) = LoggerT $ liftA2 combineLogs mf ma
    where
      combineLogs (Logged logsF f) (Logged logsA a) = Logged (logsF ++ logsA) (f a)

instance Monad m => Monad (LoggerT m) where
  return = pure
  (LoggerT ma) >>= f = LoggerT $ do
    Logged logsA a <- ma
    let (LoggerT mb) = f a
    Logged logsB b <- mb
    pure $ Logged (logsA ++ logsB) b

instance MonadFail m => MonadFail (LoggerT m) where
  fail msg = LoggerT $ fail msg
-------------------------------------------------------------------------------

-- | 2. Реализуйте функцию, позволяющую записать что-то в лог внутри трансформера 'LoggerT' (0,25 балла)
--
writeLog :: Monad m => LoggingLevel -> String -> LoggerT m ()
writeLog level msg = LoggerT $ do
  logged <- runLoggerT $ return ()  -- получаем существующий лог без изменения значения
  let newLogEntry = (level, msg)
  return $ logged { logs = logs logged ++ [newLogEntry] }

exampleComputation :: LoggerT Identity Int
exampleComputation = do
  writeLog Info "Starting the computation"
  let result = 42 + 58
  writeLog Debug "Intermediate result calculated"
  writeLog Info "Computation completed"
  return result

runExample :: (Int, [(LoggingLevel, String)])
runExample =
  let Logged logs' result = runIdentity $ runLoggerT exampleComputation
  in (result, logs')
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
loggingModification def p f = do
  currentState <- get
  lift $ writeLog Info "Performing state modification"
  let modifiedState = f currentState
  put modifiedState
  lift $ writeLog Info "State modification complete"

  if p modifiedState
    then do
      lift $ writeLog Info "State satisfies the predicate"
      return (Just modifiedState)
    else do
      lift $ writeLog Info "State does not satisfy the predicate, resetting to default"
      put def
      return Nothing

------------------------------------------------------------------------

-- | 4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'
--      и реализуйте функцию `modifyingLogging`, которая делает то же самое,
--      что и @loggingModification@ (1 балл)

instance MonadTrans LoggerT where
  lift ma = LoggerT $ do
    Logged [] <$> ma

modifyingLogging :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) ()
modifyingLogging def p f = do
  currentState <- lift get
  writeLog Info "Performing state modification"
  let modifiedState = f currentState
  lift $ put modifiedState
  writeLog Info "State modification complete"

  if p modifiedState
    then writeLog Info "State satisfies the predicate"
    else do
      writeLog Info "State does not satisfy the predicate, resetting to default"
      lift $ put def
      return ()

-------------------------------------------------------------------------------

-- | 5. Сделайте так, чтобы была возможность обращаться к `LoggerT`
--      по интерфейсу `MonadState`в случае, если трансформируемая монада сама является `MonadState`.
--
--      Реализуйте функцию `modifyingLogging'`, которая делает то же самое,
--      что и @modifyingLogging@, но используя интерфейс 'MonadTrans'. (1 балл)
--

instance MonadState s m => MonadState s (LoggerT m) where
  put :: MonadState s m => s -> LoggerT m ()
  put s = lift $ put s

  get :: MonadState s m =>  LoggerT m s
  get = lift get

modifyingLogging' :: MonadState s m => s -> (s -> Bool) -> (s -> s) -> LoggerT m ()
modifyingLogging' def p f = do
  currentState <- get
  writeLog Info "Performing state modification"
  let modifiedState = f currentState
  put modifiedState
  writeLog Info "State modification complete"

  if p modifiedState
    then writeLog Info "State satisfies the predicate"
    else do
      writeLog Info "State does not satisfy the predicate, resetting to default"
      put def
      return ()
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
  log level msg = LoggerT $ return $ Logged [(level, msg)] ()

instance (MonadTrans t, Monad m, MonadLogger m) => MonadLogger (t m) where
  log level msg = lift $ log level msg

loggingModification' :: (MonadLogger m, Monad m) => s -> (s -> Bool) -> (s -> s) -> StateT s m (Maybe s)
loggingModification' def p f = do
  currentState <- get
  log Info "Performing state modification"
  let modifiedState = f currentState
  put modifiedState
  log Info "State modification complete"

  if p modifiedState
    then do
      log Info "State satisfies the predicate"
      return (Just modifiedState)
    else do
      log Info "State does not satisfy the predicate, resetting to default"
      put def
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
  ExceptT errorType (LoggerT (StateT stateType IO)) resType


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

newtype YourState = YourState { usersDB :: [(UserId, [AccessRights])] } 
data YourError = DBReadError | CommandError deriving Show


-------------------------------------------------------------------------------
