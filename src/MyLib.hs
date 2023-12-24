{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib where

-- import Control.Monad.State    (State, StateT, MonadState (..))
import Control.Monad.Identity (Identity)
-- import Control.Monad.Trans    (MonadTrans (..))
import Control.Monad.State.Lazy
import Control.Monad.Except

-------------------------------------------------------------------------------

-- | Сегодня мы будем писать свой модный логгер!
--
data Logged a
  = Logged
      { logs :: [(LoggingLevel, String)] -- накопленный лог
      , val  :: a                        -- значение
      }
    deriving (Show, Eq)

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

instance Functor Logged where
    fmap :: (a -> b) -> Logged a -> Logged b
    fmap f (Logged acc v) = Logged acc $ f v

instance Applicative Logged where
    pure :: a -> Logged a
    pure v = Logged {logs=[], val=v}

    (<*>) :: Logged (a -> b) -> Logged a -> Logged b
    (<*>) (Logged list1 f) (Logged list2 x) = Logged (list1 <> list2) (f x)

instance Monad Logged where
    (>>=) :: Logged a -> (a -> Logged b) -> Logged b
    (>>=) (Logged log1 x) k = Logged {logs = log1 <> log2, val=y}
        where
            Logged log2 y = k x


instance Functor m => Functor (LoggerT m) where
  fmap :: Functor m => (a -> b) -> LoggerT m a -> LoggerT m b
  fmap f logT = LoggerT $ fmap (fmap f) (runLoggerT logT)

instance Monad m => Applicative (LoggerT m) where
  pure :: Applicative m => a -> LoggerT m a
  pure = LoggerT . pure . pure
  (<*>) :: Monad m => LoggerT m (a -> b) -> LoggerT m a -> LoggerT m b
  (<*>) fL xL = LoggerT $ do
    let fm = runLoggerT fL
        xm = runLoggerT xL
    f <- fm
    x <- xm
    pure $ f <*> x

instance Monad m => Monad (LoggerT m) where
  (>>=) :: Monad m => LoggerT m a -> (a -> LoggerT m b) -> LoggerT m b
  (>>=) (LoggerT xL) k = LoggerT $ do
    (Logged l1 x) <- xL
    let bL = k x 
    (Logged l2 y) <- runLoggerT bL
    pure $ Logged (l1 <> l2) y

instance MonadFail m => MonadFail (LoggerT m) where
  fail :: MonadFail m => String -> LoggerT m a
  fail = LoggerT . fail

-- getLogs :: Monad m => LoggerT m () -> LoggerT m [(LoggingLevel, String)]
-- getLogs (LoggerT xL) = LoggerT $ do
--   (Logged logs' _) <- xL
--   pure $ Logged logs' logs'

-------------------------------------------------------------------------------

-- | 2. Реализуйте функцию, позволяющую записать что-то в лог внутри трансформера 'LoggerT' (0,25 балла)
--
writeLog :: Monad m => LoggingLevel -> String -> LoggerT m ()
writeLog level str = LoggerT . pure $ Logged {logs=[(level, str)], val=()}

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
    modify f
    st <- get
    if p st 
        then do
            lift $ writeLog Info "Return correct state."
            pure $ Just st 
        else do
            put def
            lift $ writeLog Info "The state has been changed to default."
            pure Nothing

-------------------------------------------------------------------------------

-- | 4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'
--      и реализуйте функцию `modifyingLogging`, которая делает то же самое,
--      что и @loggingModification@ (1 балл)

instance MonadTrans LoggerT where
  lift :: (Monad m) => m a -> LoggerT m a
  lift xM = LoggerT $ do 
    x <- xM
    pure $ Logged {logs=[], val=x}

modifyingLogging :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) ()
modifyingLogging def p f = do
    lift $ modify f
    st <- lift get
    if p st
        then writeLog Info "Return correct state."
        else do
            lift . put $ def
            writeLog Info "The state has been changed to default."

-------------------------------------------------------------------------------

-- | 5. Сделайте так, чтобы была возможность обращаться к `LoggerT`
--      по интерфейсу `MonadState`в случае, если трансформируемая монада сама является `MonadState`.
--
--      Реализуйте функцию `modifyingLogging'`, которая делает то же самое,
--      что и @modifyingLogging@, но используя интерфейс 'MonadTrans'. (1 балл)
--

instance MonadState s m => MonadState s (LoggerT m) where
  put :: MonadState s m => s -> LoggerT m ()
  put = lift . put
  get :: MonadState s m => LoggerT m s
  get = lift get

modifyingLogging' :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) ()
modifyingLogging' def p f = do
    modify f
    st <- get
    if p st
        then writeLog Info "Return correct state."
        else do
            put def
            writeLog Info "The state has been changed to default."

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
  log' :: LoggingLevel -> String -> m ()

instance Monad m => MonadLogger (LoggerT m) where
  log' :: LoggingLevel -> String -> LoggerT m ()
  log' = writeLog

instance (MonadTrans t, Monad m) => MonadLogger (t (LoggerT m)) where
  log' :: (MonadTrans t, Monad m) => LoggingLevel -> String -> t (LoggerT m) ()
  log' level = lift . writeLog level

loggingModification' :: s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification' def p f = do
    modify f
    st <- get
    if p st
        then do
            log' Info "Return correct state."
            pure $ Just st
        else do
            put def
            log' Info "The state has been changed to default."
            pure Nothing

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

-- я решила, что важнее всего знать, кинулось ли исключение, значит Except внешняя монада
-- потом Logger, потому что логи - это хорошо))
-- далее - нам в любой ситуации хочется знать состояние, значит следующим идет StateT
-- IO очевидно последняя (т.к. она не трансформер и всё, что мы от неё хотим - работать в input/output)

-------------------------------------------------------------------------------