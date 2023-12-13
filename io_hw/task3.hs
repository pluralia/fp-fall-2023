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
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Data.List (isSuffixOf, isPrefixOf)

-- | Главная функция
main :: IO ()
main = do
  -- Получаем аргументы командной строки
  args <- getArgs
  case args of
    [dir, pattern'] -> findFilesWithPattern dir pattern'
    _               -> putStrLn "Usage: ./task3 <directory> <pattern>"

-- | Рекурсивный поиск файлов в директории
findFilesWithPattern :: FilePath -> String -> IO ()
findFilesWithPattern dir pattern' = do
  -- Проверяем, существует ли директория
  dirExists <- doesDirectoryExist dir
  if dirExists
    then do
      -- Получаем список элементов в директории
      contents <- listDirectory dir
      -- Фильтруем элементы согласно заданному шаблону
      let matchingContents = filter (matchPattern pattern') contents
      -- Выводим пути к файлам
      mapM_ (\file -> putStrLn $ dir </> file) matchingContents
      -- Рекурсивно вызываем для поддиректорий
      let subdirs = filter (\element -> element /= "." && element /= ".." && (dir </> element) `elem` matchingContents) contents
      mapM_ (\subdir -> findFilesWithPattern (dir </> subdir) pattern') subdirs
    else putStrLn $ "Directory not found: " ++ dir

-- | Проверка соответствия строки шаблону
matchPattern :: String -> FilePath -> Bool
matchPattern pattern' fileName =
  case pattern' of
    '*':suffix   -> suffix   `isSuffixOf` fileName -- заменил на библиотечную функцию
    prefix:'*':_ -> [prefix] `isPrefixOf` fileName -- и тут
    _ -> fileName == pattern'
