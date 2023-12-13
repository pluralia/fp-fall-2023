module Main where

-- find (1,5 балл)

-- Реализуйте скрипт, который рекурсивно обходит заданную директорию и все её поддиректории и выводит пути до файлов,
-- имена которых удовлетворяют заданному шаблону.

-- Шаблон может представлять из себя одну из следующих строк:
--    1. "*<строка>" —- матчит любую строку, которая заканчивается на "<строка>"
--    2. "<строка>*" —- матчит любую строку, которая начинается на "<строка>"
--    3. "<строка>"  -— матчит любую строку, которая совпадает со строкой "<строка>"
-- Во всех случаях <строка> может быть пустой.

-- Пример работы скрипта:

-- $> ./task3 /path/to/dir "*.hs"
-- /path/to/dir/task.hs
-- /path/to/dir/path/to/subdir/task_1.hs


-- Функция getArgs позволяет получить список аргументов, с которыми был запущен хаскельный скрипт.
--
import System.Environment (getArgs) -- Для обработки аргументов командной строки
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (joinPath, takeFileName, (</>))
import Control.Monad (filterM, when)
import Data.List(isPrefixOf, isSuffixOf)


matchesPattern :: String -> FilePath -> Bool
matchesPattern pattern filename
    | head pattern == '*' = tail pattern `isSuffixOf` filename -- если * стояла в начале паттерна
    | last pattern == '*' = init pattern `isPrefixOf` filename -- если * стояла в конце паттерна
    | otherwise           = pattern == filename -- если * не было, то проверяем на полное совпадение имен


searchFiles :: FilePath -> String -> IO ()
searchFiles path pattern = do
    isDirectory <- doesDirectoryExist path -- Проверяем, директория ли указана
    if isDirectory then do -- если да, то продолжаем поиск внутри
        contents <- listDirectory path  --
        let absContents = map (\entry -> path </> entry) contents -- используем оператор </> для соединения путей

        files <- filterM doesFileExist absContents -- отделяем файлы
        dirs <- filterM doesDirectoryExist absContents -- отделяем директории

        let matchingFiles = filter (matchesPattern pattern . takeFileName) files -- выбираем только те файлы, что матчатся с паттерном
        mapM_ putStrLn matchingFiles -- печатаем их

        mapM_ (searchFiles pattern) dirs -- рекурсивно спускаемся в поддиректории
    else when (matchesPattern pattern $ takeFileName path) $ putStrLn path --если там был файл, то проверяем его и если он подходит, то печатаем путь

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path, pattern] -> searchFiles path pattern
        _               -> putStrLn "Wrong arguments format, usage: myprogram2 /path/to/dir pattern"

-- компилировал так: ghc -o myprogram2 -package base -package directory -package filepath io_hw/task3.hs