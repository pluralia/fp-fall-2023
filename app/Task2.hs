module Task2 where
    
import Data.Char (isSpace)

-- wc (2 балла)

-- Реализуйте скрипт, который работает так же, как Unix-овский скрипт wc: выводит число слов, строк и байт в файле. 
-- Никакие дополнительные параметры wc поддерживать не надо.

-- Скрипт должен работать за один проход по файлу.

-- Пример работы скрипта:

-- $> ./task2 example.txt
--         9        97       640 ../example.txt

-- | Для представления файла как потока байт в Haskell есть тип `ByteString`.
--   Он может пригодиться вам для подсчёта числа байт в файле.

-- В документации сказано "All Chars will be truncated to 8 bits", значит один символ закодирован одним байтом.
-- https://hackage.haskell.org/package/bytestring-0.12.0.2/docs/Data-ByteString-Char8.html#v:pack
--

main :: IO ()
main = do
    fileName <- getLine
    str <- readFile fileName 
    let (l, w, b) = helper str 0 (0, 0, 0)
    putStrLn $ "\t" ++ show l ++ "\t" ++ show w ++ "\t" ++ show b
    where
        helper :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
        helper s ind (line, word, byte)
            | ind == length s = (newLine s ind line, newWord s ind word, byte)
            | otherwise       = helper s (ind + 1) (newLine s ind line, newWord s ind word, byte + 1)
        newLine :: String -> Int -> Int -> Int
        newLine s' i l = if (i == length s' || (s' !! i) == '\n') && i /= 0 && (s' !! (i - 1)) /= '\n'
                            then l + 1
                            else l
        newWord :: String -> Int -> Int -> Int
        newWord s' i w = if (i == length s' || isSpace (s' !! i)) && i /= 0 && not (isSpace (s' !! (i - 1)))
                            then w + 1
                            else w
