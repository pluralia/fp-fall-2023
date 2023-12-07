module Main where

import System.IO
-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользователський ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если колчиество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

loop :: Int -> Int -> IO ()
loop n i = do
    input <- getLine
    appendFile ("text_copy_" ++ show n ++ ".log") (input ++ "\n")
    putStrLn input
    let nextI = i + 1
    if nextI >= 1000
        then loop (n + 1) 0
        else loop n nextI


main :: IO ()
main = loop 0 0

