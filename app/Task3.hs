module Task3 where

import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (splitFileName)
import Control.Monad (when)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Foldable (traverse_)

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


main :: IO ()
main = do
    [startPath, pat] <- getArgs
    helper (startPath :: FilePath) pat

    where
        helper :: FilePath -> String -> IO ()
        helper path pat' = do
            flagDir <- doesDirectoryExist path
            if flagDir
                then do
                    ls <- listDirectory path
                    traverse_ (\ f -> helper (path ++ "/" ++ f) pat') ls
                else do
                    let flagCurFile = isCurrentFile (snd . splitFileName $ path) pat'
                    when flagCurFile $ putStrLn path

        isCurrentFile :: String -> String -> Bool
        isCurrentFile str pat'
          | pat' == ""                        = True
          | pat' !! (length pat' - 1) == '*'  = init pat' `isPrefixOf` str
          | head pat' == '*'                  = tail pat' `isSuffixOf` str
          | otherwise                         = str == pat'
