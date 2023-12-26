module Task2 where

import System.Environment
import System.IO
import Data.List.Split (wordsBy)
import qualified Data.ByteString.Char8 as BS
import Text.Printf

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
wc :: FilePath -> IO ()
wc file = do
    content <- BS.readFile file
    let bytes = BS.length content
        lines = length $ BS.lines content
        words = length $ wordsBy (\c -> c == ' ' || c == '\n' || c == '\t') (BS.unpack content)
    putStrLn $ printf "%8d %8d %8d %s" lines words bytes file

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> wc file
        _ -> putStrLn "./Task2 filename"
