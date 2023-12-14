module Main where
import System.IO

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользователський ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если колчиество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

genFileName :: Int -> String
genFileName n = "text_copy_" ++ show n ++ ".log"

main :: IO ()
main = do
  putStrLn "Enter your text:"
  loop 0 0
  where 
    loop :: Int -> Int -> IO ()
    loop n m = do
      text <- getLine
      putStrLn text
      file <- openFile (genFileName n) AppendMode
      hPutStrLn file text
      hClose file
      if m == 999
        then loop (n + 1) 0
        else loop n (m + 1)