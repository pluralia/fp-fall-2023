module Main where

-- wc (2 балла)

-- Реализуйте скрипт, который работает так же, как Unix-овский скрипт wc: выводит число слов, строк и байт в файле. 
-- Никакие дополнительные параметры wc поддерживать не надо.

-- Скрипт должен работать за один проход по файлу.

-- Пример работы скрипта:

-- $> ./task2 example.txt
--         9        97       640 ../example.txt

-- | Для представления файла как потока байт в Haskell есть тип `ByteString`.
--   Он может пригодиться вам для подсчёта числа байт в файле.
--
import System.Environment (getArgs)
import Data.ByteString 
import Data.Text as T
import Data.Text.Encoding as E

main :: IO ()
main = do
    args <- getArgs
    let filePath = Prelude.head args
    doStuff filePath
    

doStuff :: String -> IO()
doStuff filePath = do
    content <- Data.ByteString.readFile filePath
    let text = E.decodeUtf8 content
    let linesCount = T.count(T.pack "\n") text
    let wordsCount = (Prelude.length . T.words) text
    let bytesCount = Data.ByteString.length content
    putStrLn $ "\t " ++ "\t " ++ show   linesCount ++ "\t " ++ show wordsCount ++ "\t " ++ show bytesCount ++ "\t " ++ filePath
