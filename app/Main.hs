{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Main where

import           Objects
import           Logging
import           Database
import qualified Data.Map                                   as M
import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           System.Environment (getArgs)
import           Data.Time (getCurrentTime)


-- | 8. Реализуйте функцию-программу `processUserDB`, которая:
--        1. Сначала считывает из файла базу пользователей. 
--           Список пользователей в процессе исполнения программы должен храниться в стейте.
--           После считывания файла должна появиться соответствующая запись в логе.
--        2. Считывает из stdin команды вида "GIVE ACCESS <user_id> <access_rights>".
--           Каждая команда назначает пользователю <user_id> заданные права <access_rights>.
--           О назначении прав конкретному пользователю должна появляться запись в логе.
--        3. Если в stdin приходит команда "STOP", то считывание команд прекращается.
--           Действите тоже долно быть залогировано.
--        4. После окончания считывания команд должна произойти запись в файлы:
--             - лога программы,
--             - пользователей с назначенными им правами.
--
--     Если какое-то действие совершить не удаётся, то должна быть выброшена не IO-шная ошибка
--     и произведена запись в лог. При этом ошибка должна обрывать исполнение программы.
--     Важно, что при завершении программы из-за ошибки должна произойти запись лога в файл.
--   

main :: IO ()
main = do
  [pathToInputDB, pathToOutLog, pathToOutDB] <- getArgs
  currentTime <- getCurrentTime
  let loggerFunction = writeToLogFile pathToOutLog
  loggerFunction Empty ""
  loggerFunction Debug $ "Start session: \t" ++ show currentTime

  (Logged _ exceptRes, _) <- runStateT (runLoggerT (runExceptT (processUserDB pathToInputDB loggerFunction pathToOutDB))) (DB M.empty)
  case exceptRes of
    Left err -> do
      loggerFunction Error $ show err
      print err
    Right _ -> do
      let resultMessage = "Congratulations! Your session is over"
          resultFiles = "The result is saved to file " ++ pathToOutDB ++ ", the login is saved to file " ++ pathToOutLog
      loggerFunction Debug resultMessage
      loggerFunction Debug resultFiles
      putStrLn $ resultMessage ++ "\n" ++ resultFiles
