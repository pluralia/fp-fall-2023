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
import System.Environment
import qualified GHC.Int
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

countWords :: B.ByteString -> GHC.Int.Int64
countWords = BC.count ' ' . BC.filter (/= '\n')

main :: IO ()
main = do
  (filename : _) <- getArgs
  content <- B.readFile filename
  let 
    linesCount = BC.count '\n' content
    wordsCount = countWords content
    bytesCount = B.length content
  putStrLn $ show linesCount ++ " " ++ show wordsCount ++ " " ++ show bytesCount ++ " " ++ filename

-- To run:
-- cabal run task2 ../example.txt