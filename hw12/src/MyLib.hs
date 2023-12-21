{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module MyLib where


import Control.Monad.State    
import Control.Monad.Identity (Identity)
import Control.Monad.Trans    (MonadTrans (..))
import Prelude hiding         (log)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad
-------------------------------------------------------------------------------

-- | Сегодня мы будем писать свой модный логгер!
--
data Logged a 
  = Logged
      { logs :: [(LoggingLevel, String)] -- накопленный лог
      , val  :: a                        -- значение
      } deriving(Show, Eq)

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
    fmap  f (LoggerT m) = LoggerT $ fmap (\(Logged logs' a) -> Logged logs' (f a)) m

instance Monad m => Applicative (LoggerT m) where
    pure :: a -> LoggerT m a
    pure a = LoggerT $ pure (Logged [] a)

    (<*>) :: LoggerT m (a -> b) -> LoggerT m a -> LoggerT m b
    LoggerT mf <*> LoggerT ma = LoggerT $ do
        (Logged logs1 f) <- mf
        (Logged logs2 a) <- ma
        return $ Logged (logs1 ++ logs2) (f a)

instance Monad m => Monad (LoggerT m) where
    return :: a -> LoggerT m a
    return = pure

    (>>=) :: LoggerT m a -> (a -> LoggerT m b) -> LoggerT m b
    LoggerT ma >>= f = LoggerT $ do
        (Logged logs1 a) <- ma
        (Logged logs2 b) <- runLoggerT (f a)
        return $ Logged (logs1 ++ logs2) b

instance MonadFail m => MonadFail (LoggerT m) where
    fail :: String -> LoggerT m a
    fail msg = LoggerT $ return (Logged [(Error, msg)] (error msg))

-------------------------------------------------------------------------------

-- | 2. Реализуйте функцию, позволяющую записать что-то в лог внутри трансформера 'LoggerT' (0,25 балла)
--
wrtiteLog :: Monad m => LoggingLevel -> String -> LoggerT m ()
wrtiteLog level msg = LoggerT $ return (Logged [(level, msg)] ())

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
  -- Изменяет состояние
  _ <- modify f
  _ <- lift $ wrtiteLog Info "State was modified"

  -- Считывает его
  s <- get
  lift $ wrtiteLog Info "State was read"

  -- Если состояние удовлетворяет переданному предикату, то возвращает значение состояния
  if p s
    then do
      lift $ wrtiteLog Info "State satisfies predicate"
      return $ Just s
-- Если не удовлетворяет, то заменяет состояние дефолтным значением и возвращает Nothing
    else do
      lift $ wrtiteLog Info "State does not satisfy predicate, replacing with default"
      put def
      return Nothing

-------------------------------------------------------------------------------

-- | 4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'
--      и реализуйте функцию `modifyingLogging`, которая делает то же самое,
--      что и @loggingModification@ (1 балл)

instance MonadTrans LoggerT where
  lift :: Monad m => m a -> LoggerT m a
  lift ma = LoggerT $ fmap (Logged []) ma

modifyingLogging :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) ()
modifyingLogging def p f = do
  -- Изменяет состояние
  lift $ modify f
  _ <- wrtiteLog Info "State was modified"

  -- Считывает его
  s <- lift get
  wrtiteLog Info "State was read"

  -- Если состояние удовлетворяет переданному предикату, то возвращает значение состояния
  if p s
    then do
      wrtiteLog Info "State satisfies predicate"
-- Если не удовлетворяет, то заменяет состояние дефолтным значением и возвращает Nothing
    else do
      wrtiteLog Info "State does not satisfy predicate, replacing with default"
      put def
  return () -- не возвращаем Nothing, так как больше у нас не Maybe s

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
  get :: MonadState s m => LoggerT m s
  get = lift get

modifyingLogging' :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) ()
modifyingLogging' def p f = do
  -- Изменяет состояние
  lift $ modify f
  wrtiteLog Info "State was modified"

  -- Считывает его
  s <- lift get
  wrtiteLog Info "State was read"

  -- Если состояние удовлетворяет переданному предикату, то возвращает значение состояния
  if p s
    then do
      wrtiteLog Info "State satisfies predicate"
-- Если не удовлетворяет, то заменяет состояние дефолтным значением и возвращает Nothing
    else do
      wrtiteLog Info "State does not satisfy predicate, replacing with default"
      lift $ put def
  

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
  log :: Monad m => LoggingLevel -> String -> LoggerT m ()
  log = wrtiteLog

instance (MonadTrans t, Monad m) => MonadLogger (t (LoggerT m)) where
  log :: (MonadTrans t, Monad m) => LoggingLevel -> String -> t (LoggerT m) ()
  log level msg = lift $ log level msg

loggingModification' :: s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification' def p f = do
  -- Изменяет состояние
  modify f
  log Info "State was modified"

  -- Считывает его
  s <- get
  log Info "State was read"

  -- Если состояние удовлетворяет переданному предикату, то возвращает значение состояния
  if p s
    then do
      log Info "State satisfies predicate"
      return $ Just s
-- Если не удовлетворяет, то заменяет состояние дефолтным значением и возвращает Nothing
    else do
      log Info "State does not satisfy predicate, replacing with default"
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
newtype LoggingStateWithErrorInIO stateType errorType resType = 
  LoggingStateWithErrorInIO { runLoggingStateWithErrorInIO :: LoggerT (ExceptT errorType (StateT stateType IO)) resType }


instance Monad m => MonadLogger (LoggingStateWithErrorInIO stateType errorType) where
  log :: Monad m => LoggingLevel -> String -> LoggingStateWithErrorInIO stateType errorType ()
  log level msg = LoggingStateWithErrorInIO $ log level msg

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

-- processUserDB :: FilePath -> FilePath -> FilePath -> LoggingStateWithErrorInIO YourState YourError ()
-- processUserDB pathToInputDB pathToLog pathToOutDB = do
--     -- Сначала считывает из файла базу пользователей. 
-- --           Список пользователей в процессе исполнения программы должен храниться в стейте.
-- --           После считывания файла должна появиться соответствующая запись в логе.
--     contents <- liftIO $ readFile pathToInputDB
--     let users = map parseUser . lines $ contents
--     _ <- lift $ put users
--     _ <- log Info "User database read from file"

-- --Считывает из stdin команды вида "GIVE ACCES <user_id> <access_rights>".
-- --           Каждая команда назначает пользователю <user_id> заданные права <access_rights>.
-- --           О назначении прав конкретному пользователю должна появляться запись в логе.

--     command <- liftIO getLine
--     _ <- processCommand command

-- -- После окончания считывания команд должна произойти запись в файлы:
-- --             - лога программы,
-- --             - пользователей с назначенными им правами
--     updatedUsers <- lift . lift $ get
--     liftIO $ writeFile pathToOutDB (show updatedUsers)
--     log Info "User database written to file"
    
-- parseUser :: String -> (UserId, AccessRights)
-- parseUser str = let [userIdStr, accessRightsStr] = splitOn " " str
--                 in (read userIdStr, read accessRightsStr)

-- processCommand :: String -> LoggingStateWithErrorInIO YourState YourError ()
-- processCommand "STOP" = log Info "Command processing stopped"
-- processCommand str = case splitOn " " str of
--     ["GIVE", "ACCESS", userIdStr, accessRightsStr] -> do
--         let userId = read userIdStr
--         let accessRights = read accessRightsStr
--         _ <- loggingModification' users (\u -> fst u == userId) (\u -> (fst u, accessRights))
--         log Info $ "Access rights updated for user " ++ userIdStr
--     _ -> throwError CommandError
-------------------------------------------------------------------------------