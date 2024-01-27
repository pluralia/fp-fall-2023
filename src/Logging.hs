{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, InstanceSigs #-}

module Logging where
    
import Control.Monad.Identity (Identity)
import Control.Monad.State.Lazy
import Control.Monad.Except


type LoggerFunction = LoggingLevel -> String -> IO ()

writeToLogFile :: FilePath -> LoggerFunction
writeToLogFile pathToOutLog logLevel logMessage = appendFile pathToOutLog $ (show logLevel ++ logMessage) ++ "\n"

data Logged a
  = Logged
      { logs :: [(LoggingLevel, String)] -- накопленный лог
      , val  :: a                        -- значение
      }
    deriving (Show, Eq)

data LoggingLevel = Empty | Debug | Info | Warning | Error
  deriving (Eq)

instance Show LoggingLevel where
  show :: LoggingLevel -> String
  show Empty   = ""
  show Debug   = "Debug: \t\t"
  show Info    = "Info: \t\t"
  show Warning = "Warning: \t"
  show Error   = "Error: \t\t"

newtype LoggerT m a = LoggerT { runLoggerT :: m (Logged a) }

type Logger a = LoggerT Identity a

class MonadLogger m where
  log' :: LoggingLevel -> String -> m ()

type LoggingStateWithErrorInIO stateType errorType resType = 
    ExceptT errorType (LoggerT (StateT stateType IO)) resType


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

instance MonadTrans LoggerT where
  lift :: (Monad m) => m a -> LoggerT m a
  lift xM = LoggerT $ fmap (Logged []) xM

instance (MonadIO m) => MonadIO (LoggerT m) where
    liftIO :: MonadIO m => IO a -> LoggerT m a
    liftIO = lift . liftIO

instance MonadState s m => MonadState s (LoggerT m) where
  put :: MonadState s m => s -> LoggerT m ()
  put = lift . put
  get :: MonadState s m => LoggerT m s
  get = lift get

instance Monad m => MonadLogger (LoggerT m) where
  log' :: LoggingLevel -> String -> LoggerT m ()
  log' level str = LoggerT . pure $ Logged [(level, str)] ()

instance (MonadTrans t, Monad m) => MonadLogger (t (LoggerT m)) where
  log' :: (MonadTrans t, Monad m) => LoggingLevel -> String -> t (LoggerT m) ()
  log' level str = lift . LoggerT . pure $ Logged [(level, str)] ()
