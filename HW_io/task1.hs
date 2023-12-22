module Main where

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользователський ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если колчиество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

import System.IO

main :: IO ()
main = do
    infLoop 0 0

infLoop :: Integer -> Integer -> IO()
infLoop n count = do
    line <- getLine

    putStrLn line

    let logFile = "text_copy_" <> show n <> ".log"
    -- https://hackage.haskell.org/package/base-4.19.0.0/docs/System-IO.html#v:withFile
    -- как with open('file.txt):
    withFile logFile AppendMode $ \handle -> do
        hPutStrLn handle line
    
    let newN = if count == 999
        then n + 1
        else n
    let newCount = if count == 999
        then 0
        else count

    infLoop newN newCount
