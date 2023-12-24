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
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    [path] <- getArgs
    wcFunc path
      where
        wcFunc :: FilePath -> IO ()
        wcFunc curPath = do
            content <- B.readFile curPath
            let linesCount = length (lines $ B.unpack content)
                wordsCount = length (words $ B.unpack content)
                bytesCount = B.length content
            putStrLn $ "\t" ++ (show linesCount) ++ "\t" ++ (show wordsCount) ++ "\t" ++ (show bytesCount) ++ "\t" ++ curPath