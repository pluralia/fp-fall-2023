module Database where

import           Objects
import           Parser
import           Errors
import           Logging
import qualified Data.Map                  as M
import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           System.IO
import qualified Data.Set                  as S
import           Data.List
import           Data.Char (toUpper)


writeToFileAndLogger :: Bool -> LoggerFunction -> LoggingLevel -> String -> LoggingStateWithErrorInIO MyState MyError ()
-- третий параметр - печатать ли сообщение в консоль
writeToFileAndLogger True loggerFunction logLevel logMessage 
  | logLevel == Error || logLevel == Warning  = do
    log' logLevel logMessage
    liftIO $ loggerFunction logLevel logMessage
    liftIO $ putStrLn $ map toUpper (show logLevel) ++ logMessage
  | otherwise                                 = do
    log' logLevel logMessage
    liftIO $ loggerFunction logLevel logMessage
    liftIO $ putStrLn logMessage
writeToFileAndLogger False loggerFunction logLevel logMessage = do
  log' logLevel logMessage
  liftIO $ loggerFunction logLevel logMessage

processUserDB :: FilePath-> LoggerFunction  -> FilePath -> LoggingStateWithErrorInIO MyState MyError ()
processUserDB pathToInputDB loggerFunction pathToOutDB = do
    userDB <- liftIO $ readFile pathToInputDB
    case userDB of
      "" -> do
        writeToFileAndLogger True loggerFunction Error "The file with UserDB is empty!"
        throwError (EmptyFile pathToInputDB)
      str -> case runParser dataBaseP str of
        Just (db, "") -> do
          writeToFileAndLogger True loggerFunction Info "The file was read. User's data base was created!"
          liftIO $ putStrLn "Enter a command like: 'GIVE ACCESS <user ID> <access right>'"
          put db
        _      -> do
          writeToFileAndLogger True loggerFunction Error "The file couldn't be parsed!"
          throwError IncorrectFile
          
    -- Создали базу данных пользователей. Теперь считаем команды и раздадим права всем нуждающимся :)
    command <- liftIO getLine
    executeCommands command
    myDB <- get
    liftIO $ writeDBFile myDB

    where
      executeCommands :: String -> LoggingStateWithErrorInIO MyState MyError ()
      executeCommands command = do
        if command == "STOP"
          then do -- завершили считывание команд
            let logMessage = "Completing the reading of commands with the granting of rights to users!"
            writeToFileAndLogger False loggerFunction Info logMessage
          
          else do
            case runParser commandP command of

              Just ((userID, Right userRight), "") -> do -- смогли распарсить команду
                DB db <- get

                case M.lookup userID db of -- ищем пользователя в базе данных
                  Just _  -> do
                    modify $ \_ -> DB (M.insertWith (<>) userID [userRight] db)
                    let logMessage = "Add " ++ show userRight ++ " rights for user " ++ show userID ++ "."
                    writeToFileAndLogger False loggerFunction Info logMessage
                  
                  Nothing -> writeToFileAndLogger True loggerFunction Warning $ show $ UserNotInDB userID

              Just ((_, Left logMessage), _)  -> writeToFileAndLogger True loggerFunction Warning logMessage

              _                               -> do -- не смогли распарсить команду
                writeToFileAndLogger True loggerFunction Warning $ show $ IncorrectCommand command
                liftIO $ putStrLn "If you want to finish entering commands, enter 'STOP'"

            newCommand <- lift . lift . lift $ getLine
            executeCommands newCommand

      writeDBFile :: MyState -> IO ()
      writeDBFile (DB db) = do
        withFile pathToOutDB WriteMode $ \h -> do
          let keys = M.keys db
              partResults = map (\key -> "User " ++ show key ++ " have this rules: " ++ showRights (db M.! key)) keys
              result = intercalate "\n" partResults
          hPutStrLn h result

        where
          showRights :: [AccessRights] -> String
          showRights []   = "Default"
          showRights list = unwords . map show . S.toList $ S.fromList list
