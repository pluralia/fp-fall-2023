module Task1 where

import System.IO

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользователський ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если колчиество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

createFile :: Int -> String
createFile n = "tex_copy" ++ show n ++ ".log"

checkLine ::  Int -> Int -> IO ()
checkLine n m
    | m /= 999 = logLoop n (m + 1)
    | otherwise = logLoop (n + 1) 0

logLoop :: Int -> Int -> IO ()
logLoop n m = do
    content <- getLine
    logfile <- openFile (createFile n) AppendMode
    hPutStrLn logfile content
    hClose logfile
    checkLine n m



main :: IO ()
main = logLoop 0  0
