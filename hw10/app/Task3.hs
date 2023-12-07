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
import System.Environment(getArgs)
import System.Directory(listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath((</>))
import Data.List(isPrefixOf, isSuffixOf)
import Control.Monad(when, filterM)

matchPattern :: String -> String -> Bool
matchPattern pattern filename | last pattern == '*' = init pattern `isPrefixOf` filename
                              | head pattern == '*' = tail pattern `isSuffixOf` filename
                              | otherwise           = pattern == filename

checkFile :: String -> FilePath -> IO ()
checkFile pattern file = when (matchPattern pattern file) $ putStrLn file

traverseDir :: FilePath -> String -> IO ()
traverseDir dir pattern = do
    contents <- listDirectory dir -- get contents of the current directory
    let 
      paths = map (dir </>) contents -- get full paths
    files <- filterM doesFileExist paths -- get only files
    mapM_ (checkFile pattern) files -- match pattern
    dirs <- filterM doesDirectoryExist paths -- get only directories
    mapM_ (`traverseDir` pattern) dirs -- traverse directories recursively


main :: IO ()
main = do
  (dir:pattern:_) <- getArgs
  traverseDir dir pattern

-- How to run:
-- cabal run task3 . "*.hs"
