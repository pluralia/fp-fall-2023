{-# LANGUAGE InstanceSigs, TupleSections, ScopedTypeVariables #-}

module MyBeautifulDB where

import MyLib
import Parser
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Applicative (Alternative (..))
import Data.Functor
import System.IO
-- import qualified Data.Set as S
import Data.List

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
--     (3,5 балла)
--   

-- | Тип пользовательского id.
--
type UserID = Int

-- | Тип возможных прав пользователей.
--
data AccessRights = Read | Write | ReadAndWrite | Admin
  deriving (Eq, Show)

newtype MyState = DB (M.Map UserID [AccessRights])
  deriving (Eq)

-- instance Show MyState where
--   show :: MyState -> String
--   show (DB dict) = forM_ [M.keys dict] $ \i -> do


-- ПАРСЕР файла со списком пользователей
dataBaseP :: Parser MyState
dataBaseP = DB . M.fromList . map (, [] :: [AccessRights]) <$ spaceP <*> sepBy (satisfyP (=='\n')) intP

accessRightP :: Parser AccessRights
accessRightP = (stringP "Read" $> Read) <|> (stringP "Write" $> Write)
            <|> (stringP "ReadAndWrite" $> ReadAndWrite) <|> (stringP "Admin" $> Admin)

commandP :: Parser (UserID, AccessRights)
commandP =  (,)
        <$  stringP "GIVE ACCESS "
        <*> intP
        <*  spaceP
        <*> accessRightP

-- addAccessRight :: MyState -> UserID -> AccessRights -> MyState
-- addAccessRight db user right = 
--   case M.lookup user db of
--     Just _ -> M.insertWith (<>) user [right]
--     Nothing -> throwError (UserNotInDB user)

data MyError = EmptyFile String         -- пустой файл базы пользователей
                | UncorrectFile         -- ошибка при чтении файла
                -- | UncorrectCommand      -- считывание команд с консоли
                -- | UserNotInDB UserID    -- пользователь не найден в базе данных
                -- | UncorrectAccessRight  -- некорректные права пользователя
                | ErrorWritingFile      -- на всякий случай (?) - ошибка при записи в файл

instance Show MyError where
  show :: MyError -> String
  show (EmptyFile fileName) = "The file " ++ fileName ++ " with UserDB is empty!"
  show UncorrectFile        = "Error with parsing file!"
  -- show UncorrectCommand     = "Uncorrect command!"
  -- show (UserNotInDB user)   = "User " ++ show user ++ " not in database!"
  -- show UncorrectAccessRight = "Uncorrect access rights!"
  show ErrorWritingFile     = "I have some problem with writing on file..."

-- type MyErrorMonad = Either MyError

-- throwError ... - передача ошибки

-- type LoggingStateWithErrorInIO stateType errorType resType = 
--    ExceptT errorType (LoggerT (StateT stateType IO)) resType

-- data LoggingLevel = Debug | Info | Warning | Error

processUserDB :: FilePath -> FilePath -> FilePath -> LoggingStateWithErrorInIO MyState MyError ()
processUserDB pathToInputDB _ pathToOutDB = do
    userDB <- lift . lift . lift $ readFile pathToInputDB
    case userDB of
      "" -> do
        log' Error "The file with UserDB is empty!"
        throwError (EmptyFile pathToInputDB)
      str -> case runParser dataBaseP str of
        Just (db, _) -> do
          log' Info "The file was read. User's data base was created!"
          put db
        Nothing      -> do
          log' Error "The file couldn't be parsed!"
          throwError UncorrectFile
    -- Создали базу данных пользователей. Теперь считаем команды и раздадим права всем нуждающимся :)
    command <- lift . lift . lift $ getLine
    readCommands command
    myDB <- get
    lift . lift . lift $ writeDBFile pathToOutDB myDB
    pure ()

    where
      readCommands :: String -> LoggingStateWithErrorInIO MyState MyError ()
      readCommands command = do
        if command == "STOP"
          then do -- завершили считывание команд
            log' Info "Completing the reading of commands with the granting of rights to users!"
            pure ()
          else do
            case runParser commandP command of

              Just ((userID, userRight), _) -> do -- смогли распарсить команду
                DB db <- get

                case M.lookup userID db of -- ищем пользователя в базе данных
                  Just _  -> do
                    modify $ \_ -> DB (M.insertWith (<>) userID [userRight] db)
                    log' Info $ "Congratulate! Add " ++ show userRight ++ "rights for user " ++ show userID ++ "."
                  Nothing -> log' Warning $ "User " ++ show userID ++ "not in database!"

              Nothing -> do -- не смогли распарсить команду
                log' Warning $ "Command was uncorrect: " ++ command

            newCommand <- lift . lift . lift $ getLine
            readCommands newCommand

      writeDBFile :: FilePath -> MyState -> IO ()
      writeDBFile pathToOutDB' (DB db) = do
        withFile pathToOutDB' WriteMode $ \h -> do
          let keys = M.keys db
              part_results = map (\key -> "User " ++ show key ++ " have this rules: " ++ printRights (db M.! key)) keys
              result = intercalate "\n" part_results
          hPutStrLn h result

      printRights :: [AccessRights] -> String
      printRights = foldl' (\ acc ai -> acc ++ " " ++ show ai) " "