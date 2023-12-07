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
import Data.ByteString 
import Data.Text as T
import Data.Text.Encoding as E

main :: IO ()
main = do
    content <- Data.ByteString.readFile "D:/Study/fp-fall-2023-1/hw10/text_copy_0.log"
    let text = E.decodeUtf8 content
    let linesCount = T.count(T.pack "\n") text
    let wordsCount = (Prelude.length . T.words) text
    let bytesCount = Data.ByteString.length content
    putStrLn $ "\t " ++ "\t " ++ show   linesCount ++ "\t " ++ show wordsCount ++ "\t " ++ show bytesCount ++ "\t " ++"D:/Study/fp-fall-2023-1/hw10/text_copy_0.log"
