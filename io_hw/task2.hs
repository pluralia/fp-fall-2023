module Main where

import qualified Data.ByteString      as B
import           Data.ByteString.UTF8 (toString)
import           System.Environment   (getArgs)
import           Text.Printf

-- wc (2 балла)

-- Реализуйте скрипт, который работает так же, как Unix-овский скрипт wc: выводит число слов, строк и байт в файле.
-- Никакие дополнительные параметры wc поддерживать не надо.

-- Скрипт должен работать за один проход по файлу.
-- Скрипт должен принимать путь до файла в качестве аргумента

-- Пример работы скрипта:

-- $> ./task2 example.txt
--         9        97       640 ../example.txt

-- | Для представления файла как потока байт в Haskell есть тип `ByteString`.
--   Он может пригодиться вам для подсчёта числа байт в файле.
--

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> myWC filePath
        _          -> putStrLn "I need only one argument: path to file!"

myWC :: String -> IO ()
myWC filePath = do
    bytesContent <- B.readFile filePath
    let textContent = toString bytesContent
    let ws = length . words $ textContent
    let ls = length . filter (/= "") . lines $ textContent
    let bs = B.length bytesContent
    putStrLn $ printf "%10d %10d %10d %s" ls ws bs filePath
