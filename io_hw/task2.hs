module Main where

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
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            fileContent <- BS.readFile filePath
            let linesCount = BS.count '\n' fileContent
                wordsCount = length $ BS.words fileContent
                bytesCount = BS.length fileContent
            putStrLn $ formatOutput linesCount wordsCount bytesCount filePath
        _ -> putStrLn "format `./task2 <file_path>` needed"

formatOutput :: Int -> Int -> Int -> FilePath -> String
formatOutput linesCount wordsCount bytesCount filePath =
    concat [show linesCount, "\t", show wordsCount, "\t", show bytesCount, "\t", filePath]

