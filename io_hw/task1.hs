module Main where
import           Control.Monad
import           System.IO
import           Text.Printf

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользователський ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если колчиество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

writeToLog :: Integer -> IO ()
writeToLog n = do
    let filePath = printf "text_copy_%d.log" n
    h <- openFile filePath WriteMode
    forM_ [1..1000] $ \i -> do
        inp <- getLine
        putStrLn inp
        hPutStrLn h inp
        hFlush h

    hClose h
    writeToLog $ n + 1


main :: IO ()
main = writeToLog 0
