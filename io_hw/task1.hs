module Main where

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользователський ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если колчиество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

import System.IO
import Control.Monad
import System.Directory

-- | Главная функция
main :: IO ()
main = do
    -- Создаем директорию для сохранения логов, если ее еще нет
    createDirectoryIfMissing True "logs"
    -- Запускаем цикл обработки ввода
    processInput 0

processInput :: Int -> IO ()
processInput fileNumber = do
    putStr "Enter a line: "
    hFlush stdout -- принудительно сбрасываем буфер вывода
    
    line <- getLine
    putStrLn line -- печать в stdout

    -- Открываем файл для записи
    withFile (logFileName fileNumber) AppendMode $ \handle -> do
        hPutStrLn handle line
    
    -- Проверка условия на 1000 записей
    let nextFileNumber = if fileNumber < 999 && length line == 1000
        then fileNumber + 1
            else fileNumber

    -- Рекурсивный вызов с новым номером файла
    processInput nextFileNumber

logFileName :: Int -> FilePath
logFileName fileNumber = "logs/text_copy_" <> show fileNumber <> ".log"
