module Main where

import MyLib

import Text.Megaparsec
import System.IO

main :: IO ()
main = do
  -- Открываем файл для чтения
--   handle <- openFile "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\only_atoms.pdb" ReadMode
  handle <- openFile "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\atoms_with_bonds.pdb" ReadMode
  -- Считываем содержимое файла
  contents <- hGetContents handle
  -- Парсим содержимое файла с помощью парсера pdbParser', либо поменять на pdbParser, если со связями файл
  case parse pdbParser "" contents of
    Left err -> putStrLn $ "Ошибка парсинга: " ++ errorBundlePretty err
    Right pdbData -> print pdbData