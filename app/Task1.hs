module Task1 where
    
import System.IO
import Control.Monad (forM_)

-- REPL (1,5 балла)

-- Реализуйте скрипт, который при запуске в бесконечном цикле считывает с консоли пользовательcкий ввод.
-- Каждая введённая строка должна дублироваться в stdout (терминал) и в файл с названием text_copy_<N>.log,
-- где N — номер файла с логом. Нумерация N начинается с 0.

-- Если количество записанных строк в файл text_copy_<N>.log достигло 1000,
-- то должен создаваться файл text_copy_<N+1>.log.
-- Запись последующих строк должна производиться в него, а хэндл файла text_copy_<N>.log должен быть закрыт.

main :: IO ()
main = helper 0
    where
        helper :: Integer -> IO ()
        helper n = do
            let fileName = "text_copy_" ++ show n ++ ".log"
            withFile fileName WriteMode (\h -> do           -- withFile сам закрывает файл
                forM_ [1..1000 :: Integer] $ \_ -> do
                    str <- getLine
                    putStrLn str
                    hPutStrLn h str
                    hFlush h                                -- записывает в текущем моменте, а не после завершения программы
                )
            helper $ n + 1
