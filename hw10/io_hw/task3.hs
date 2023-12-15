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


-- Функция getArgs позволяет получить список аргументов, с которыми был запущен хаскльный скрипт.
--
import System.Environment (getArgs)
import System.Directory
import System.Environment
import System.FilePath
import Data.List
import Control.Monad (forM_, filterM)

main :: IO ()
main = do
    args <- getArgs
    let (dir, pattern) = (head args, args !! 1) 
    processDirectory dir pattern

processDirectory :: FilePath -> String -> IO ()
processDirectory dir pattern = do
    contents <- listDirectory dir
    let contents' = map (dir </>) contents
    files <- filterM doesFileExist contents'
    forM_ files $ processFile pattern
    dirs <- filterM doesDirectoryExist contents'
    forM_ dirs $ \d -> processDirectory d pattern

processFile :: String -> FilePath -> IO ()
processFile pattern file =
    if matches pattern  file
        then putStrLn file
        else return ()

matches :: String -> String -> Bool
matches ('*':xs) ys = isSuffixOf xs ys
matches xs ('*':ys) = isPrefixOf xs ys
matches xs ys = xs == ys