module Main where
import GHC.IO.IOMode (IOMode(AppendMode))
import System.IO (openFile, hPutStr)
import System.Posix (OpenMode(ReadWrite))
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import Text.Printf (printf)
import System.IO (IOMode(WriteMode))
import Control.Monad (forM_)
import GHC.IO.Handle.Text (hPutStrLn)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle (hClose)

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользователський ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если колчиество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

-- main :: IO ()
-- main = undefined


-- main' :: IO ()
-- main' = do
--     putStr "What is your name?\nName: "
--     name <- getLine
--     if null name then main' else putStrLn $ "Hi, " ++ name ++ "!"

-- main' :: IO ()
-- main' = do
--     Thou_Shalt_Not_Use_This_IO.hSetBuffering stdout Thou_Shalt_Not_Use_This_IO.NoBuffering
--     putStr "Substring: "
    
--     strToDel <- getLine
--     if null strToDel then putStrLn "Canceled"
--     else do
--         dirs <- getDirectoryContents "."
--         mapM_ (\x -> if strToDel `isInfixOf` x then 
--             do
--                 removeFile x
--                 putStrLn x
--                 else return ()) dirs
--         main'

writeToLog :: Integer -> IO ()
writeToLog n = do
    let filePath = printf "text_copy_%d.log" n
    h <- openFile filePath WriteMode
    forM_ [1..1000] $ \i -> do
        inp <- getLine
        putStrLn inp
        hPutStrLn h inp
        hFlush h

    hClose h
    writeToLog $ n + 1


main :: IO ()
main = writeToLog 0
