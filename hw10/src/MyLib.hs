module MyLib where

import Control.Monad.State
import System.IO

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
--   3. Если толкнуть рычаг, когда турникет разблокирован, рычаг откроется, чтобы пропустить его, а затем закроется, чтобы не пропустить никого другого
--   4. Если толкнуть рычаг, когда турникет заблокирован, он вежливо погладит его, но не пропустит и останется запертым.

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

-- | Реализуйте автомат
--

type FSM s = State s s

fsm :: (a -> s -> s) -> a -> FSM s
fsm translate act = state $ \s -> (s, translate act s)

actions :: [TurnstileInput]
actions = [Coin, Coin, Push, Push, Coin]

turnstile :: State TurnstileState TurnstileOutput
turnstile = do
  outputs <- mapM singleAct actions
  return $ last outputs
  where 
    singleAct :: TurnstileInput -> State TurnstileState TurnstileOutput
    singleAct action = do
      state <- get
      case (action, state) of
        (Coin, Locked) -> do
          put Unlocked
          return Thank
        (Coin, Unlocked) -> return Thank
        (Push, Locked) -> return Tut
        (Push, Unlocked) -> do
          put Locked
          return Open

-- Привидите пример запуска на последовательности действий [Coin, Coin, Push, Push, Coin]
-- (можете привести свою любой длины и содержания)

res :: TurnstileOutput
res = evalState turnstile Locked

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

-- Разница в том, что в первом случае запись происходит сразу, а во втором -- с задержкой, т.к. буферизация
-- По скорости работы разницы особой я не заметил. Мб с буфером чуть медленнее

-------------------------------------------------------------------------------

-- Оставшиеся 4 задания лежат в папке io_hw

-- В рамках каждого из этих заданий нужно реализовать отдельный скрипт, который можно запутсить из консоли.
-- Заготовки для каждого задания уже созданы.

-- Перед сдачей задания проверьте, что созданные вами скрипты действительно собираются и работают.
-- Скомпилировать скрипт можно командой `ghc taskN.hs`, а запустить -- `./taskN <аргументы>`

-- Обратите внимание на функцию `printf` -- она может быть полезна для печати

-------------------------------------------------------------------------------