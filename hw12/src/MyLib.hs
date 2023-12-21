
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib where
    
import Control.Monad.State    (State, StateT, MonadState (..), gets)
import Control.Monad.Except   (ExceptT)
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

instance Functor Logged where
  fmap :: (a -> b) -> Logged a -> Logged b
  fmap f (Logged lgs v) = Logged lgs (f v)

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
  fmap f (LoggerT x) = LoggerT $ fmap (fmap f) x

instance Monad m => Applicative (LoggerT m) where
  pure :: a -> LoggerT m a
  pure = lift . pure
  
  (<*>) :: LoggerT m (a -> b) -> LoggerT m a -> LoggerT m b
  (<*>) mf mx = do
    f <- mf
    f <$> mx
    
instance Monad m => Monad (LoggerT m) where
  return :: a -> LoggerT m a
  return = pure

  (>>=) :: LoggerT m a -> (a -> LoggerT m b) -> LoggerT m b
  (>>=) (LoggerT x) f = LoggerT $ do
    Logged logs1 val1 <- x
    Logged logs2 val2 <- runLoggerT $ f val1
    return $ Logged (logs1 ++ logs2) val2

instance MonadFail m => MonadFail (LoggerT m) where
  fail :: String -> LoggerT m a
  fail = lift . fail

-------------------------------------------------------------------------------

-- | 2. Реализуйте функцию, позволяющую записать что-то в лог внутри трансформера 'LoggerT' (0,25 балла)
--
writeLog :: Monad m => LoggingLevel -> String -> LoggerT m ()
writeLog level m = LoggerT $ pure $ Logged [(level, m)] ()

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
  s <- gets f
  if p s
    then do
      return $ Just s
    else do
      put def
      return Nothing

-------------------------------------------------------------------------------

-- | 4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'
--      и реализуйте функцию `modifyingLogging`, которая делает то же самое,
--      что и @loggingModification@ (1 балл)

instance MonadTrans LoggerT where
  lift :: Monad m => m a -> LoggerT m a
  lift = LoggerT . fmap (Logged [])

modifyingLogging :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) ()
modifyingLogging def p f = do
  s <- lift $ gets f
  if p s
    then do
      return ()
    else do
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
  put :: s -> LoggerT m ()
  put = lift . put

  get :: LoggerT m s
  get = lift get

modifyingLogging' :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) ()
modifyingLogging' def p f = do
  s <- gets f
  if p s
    then do
      return ()
    else do
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
  log = writeLog

instance (MonadTrans t, Monad m) => MonadLogger (t (LoggerT m)) where
  log = (lift .) . writeLog

loggingModification' :: s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification' def p f = do
  s <- gets f
  log Info "Applying modification"
  if p s
    then do
      log Info "Modification applied"
      return $ Just s
    else do
      log Info "Modification not applied"
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
type LoggingStateWithErrorInIO stateType errorType resType = StateT stateType (ExceptT errorType (LoggerT IO)) resType

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

data User = User {
  id :: UserId,
  rights :: [AccessRights]
}

-- | Тип возможных прав пользователей.
--
data AccessRights = Read | Write | ReadAndWrite | Admin

newtype CustomState = CustomState {
  users :: [User]
}
newtype CustomError = CustomError {
  msg :: String
}

readUsers :: FilePath -> IO [User]
readUsers fp = do
  content <- readFile fp
  let 
    ids :: [UserId]
    ids = map read $ lines content 
  return $ map (`User` []) ids


processUserDB :: FilePath -> FilePath -> FilePath -> LoggingStateWithErrorInIO CustomState CustomError ()
processUserDB pathToInputDB pathToLog pathToOutDB = undefined

-------------------------------------------------------------------------------