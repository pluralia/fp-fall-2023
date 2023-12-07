module Main where

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользовательский ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если количество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

import System.IO

-- Функция, которая создает и открывает файл с логами
openLogFile :: Int -> IO Handle
openLogFile n = do
  -- создаем и открываем файл с названием вида text_copy_<N>.log
  let fileName = "text_copy_" ++ show n ++ ".log"
  openFile fileName WriteMode

-- Функция, которая записывает строку в файл с логами и закрывает его при необходимости
writeToLog :: Int -> Handle -> Int -> String -> IO Handle
writeToLog n h count s = do
  -- записываем строку s в файл
  hPutStrLn h s
  if count >= 1000
    then do
      -- если count >= 1000, мы закрываем старый файл и открываем новый
      hClose h
      openLogFile (n + 1)
    else
      -- иначе, сохраняем текущий файл
      return h

-- Функция, которая обрабатывает пользовательский ввод через рекурсивный вызов
logLoop :: Int -> Handle -> Int -> IO ()
logLoop n h count = do
  -- считываем строку ввода
  s <- getLine
  
  -- проверяем, равна ли строка слову "exit" 
  if s == "exit"
    then do -- если равна, закрываем файл и завершаем цикл
      hClose h
    else do -- если нет, продолжаем работу цикла
      -- выводим строку в терминал
      putStrLn s
      -- записываем строку в файл, получим текущий файл назад
      newH <- writeToLog n h count s
      -- рекурсивный вызов logLoop с новым значением count, если файл был смещен
      if newH == h
        then logLoop n h (count + 1)
        else logLoop (n + 1) newH 1 

main :: IO ()
main = do
  -- открываем файл text_copy_0.log для записи
  h <- openLogFile 0
  -- запускаем бесконечный цикл считывания сообщений и записи их в терминал и файл
  logLoop 0 h 1