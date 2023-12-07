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
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs
    case args of
        [filename] -> countFileStats filename
        _ -> putStrLn "Usage: ./task2 <filename>"


countFileStats :: FilePath -> IO ()
countFileStats filename = do
    -- Читаем содержимое файла в byteString
    fileContent <- BS.readFile filename
    let byteCount = BS.length fileContent
        wordCount = length $ words $ BS.unpack fileContent
        lineCount = length $ filter (not . null) $ lines $ BS.unpack fileContent -- учитываем только ненулевые строки
    
    -- Вывод результата
    putStrLn $ formatStats lineCount wordCount byteCount


formatStats :: Int -> Int-> Int -> String
formatStats lines words bytes = 
    concat [justify 9 (show lines), justify 9 (show words), justify 9 (show bytes)]

justify :: Int -> String -> String
justify width str = replicate (width - length str) ' ' <> str