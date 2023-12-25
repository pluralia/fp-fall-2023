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
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory
import Control.Monad (forM_, filterM)
import System.FilePath

getPatternMatch :: String -> String -> Bool
getPatternMatch fileName pattern
  | pattern !! (length pattern - 1) == '*' = init pattern `isPrefixOf` fileName
  | head pattern == '*' = tail pattern `isSuffixOf` fileName
  | otherwise = fileName == pattern

myFindFiles :: FilePath -> String -> IO ()
myFindFiles curDir pattern = do
    -- все, что есть в текущей директории + имя
    ls <- listDirectory curDir
    let ethFounded = map (curDir </>) ls
    -- файлы
    -- фильтр https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html#v:filterM
    foundFiles <- filterM doesFileExist ethFounded
    -- https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html#v:forM_
    forM_ foundFiles $ \f -> do
      if getPatternMatch f pattern
        then putStrLn f
        else return ()
    
    -- поддиректории
    subDirs <- filterM doesDirectoryExist ethFounded
    forM_ subDirs $ \d -> myFindFiles d pattern

main :: IO ()
main = do
    args <- getArgs
    let (dir, filePattern) = (head args, args !! 1) 
    myFindFiles dir filePattern