module MyLib () where
  
import Control.Monad.State
import System.IO
import Control.Monad (forM_)
import GHC.Base (when)

-------------------------------------------------------------------------------

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
--   3. Если толкнуть рычаг, когда турникет разблокирован, рычаг откроется, чтобы пропустить его, 
--      а затем закроется, чтобы не пропустить никого другого
--   4. Если толкнуть рычаг, когда турникет заблокирован, он вежливо погладит его, но не пропустит и останется запертым.

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

-- | Реализуйте автомат
--
switch :: TurnstileState -> TurnstileInput -> (TurnstileOutput, TurnstileState)
switch _        Coin = (Thank, Unlocked)
switch Unlocked Push = (Open,  Locked)
switch Locked   Push = (Tut,   Locked)

action :: [TurnstileInput]
action = [Coin, Coin, Push, Push, Coin]

fsm :: (TurnstileState -> TurnstileInput -> (TurnstileOutput, TurnstileState)) 
        -> TurnstileInput -> State TurnstileState TurnstileOutput
fsm trans inputAction = state $ \s -> trans s inputAction

turnstile :: State TurnstileState [TurnstileOutput]
turnstile = mapM (fsm switch) action

testRun :: ([TurnstileOutput], TurnstileState)
testRun = runState turnstile Locked

-- Привидите пример запуска на последовательности действий [Coin, Coin, Push, Push, Coin]
-- (можете привести свою любой длины и содержания)

-------------------------------------------------------------------------------

-- 2. Запись в файл с буфером и без (0,5 балла)

--    На лекции мы разбирали 2 этих функции -- запустите их и посмотрите на вывод.
--    В чем вы видите разницу? Как можете ее объяснить?

-- | запись в файл без буфера
--
writeToFile :: IO ()
writeToFile = do
    h <- openFile "test.txt" WriteMode

    forM_ [0 .. 10000000] $ \i -> do
      hPutStrLn h $ "Processing " <> show i

    hClose h

-- | запись в файл с буфером
--
writeToFileWithBuffer :: IO ()
writeToFileWithBuffer = do
    h <- openFile "test.txt" WriteMode
    hSetBuffering h LineBuffering

    forM_ [0 .. 10000000] $ \i -> do
      hPutStrLn h $ "Processing " <> show i
      when (i `mod` 10000 == 0) $ hFlush h

    hClose h

writeToFileWithoutBuffer :: IO ()
writeToFileWithoutBuffer = do
    h <- openFile "test.txt" WriteMode
    hSetBuffering h NoBuffering

    forM_ [0 .. 10000000] $ \i -> do
      hPutStrLn h $ "Processing " <> show i
      when (i `mod` 10000 == 0) $ hFlush h

    hClose h

-- Запись в файл с буфером работает дольше - это происходит из-за того, что буфер по умолчанию работает эффективнее.
-- Но обе эти функции справились за пару секунд.
-- Третья функция записывает без буфера и работает дольше 10 минут.

-------------------------------------------------------------------------------

-- Оставшиеся 4 задания лежат в папке io_hw

-- В рамках каждого из этих заданий нужно реализовать отдельный скрипт, который можно запутсить из консоли.
-- Заготовки для каждого задания уже созданы.

-- Перед сдачей задания проверьте, что созданные вами скрипты действительно собираются и работают.
-- Скомпилировать скрипт можно командой `ghc taskN.hs`, а запустить -- `./taskN <аргументы>`

-- Обратите внимание на функцию `printf` -- она может быть полезна для печати

-------------------------------------------------------------------------------
