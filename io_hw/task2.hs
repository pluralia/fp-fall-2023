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
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- Функция для подсчета строк, слов и байтов в файле
countFile :: FilePath -> IO ()
countFile path = do
  content <- BS.readFile path -- считываем содержимое файла в виде ByteString
  let linesCount = T.count (T.pack "\n") . decodeUtf8 $ content -- подсчет строк
  let wordsCount = length . T.words . decodeUtf8 $ content -- подсчет слов (подсчет количества слов, разделенных пробелами)
  let bytesCount = BS.length content -- подсчет байтов (подсчет длины ByteString)
  putStrLn $ show linesCount ++ "\t" ++ show wordsCount ++ "\t" ++ show bytesCount ++ "\t" ++ path -- вывод результатов

main :: IO ()
main = do
  let filePath = "example.txt" -- устанавливаем путь к файлу, который нужно обработать
  countFile filePath -- вызов функции подсчета строки, слов и байтов в файле
