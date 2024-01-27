{-# LANGUAGE InstanceSigs #-}

module Errors where

import Objects (UserID)

data MyError  = EmptyFile String        -- пустой файл базы пользователей
              | IncorrectFile           -- ошибка при чтении файла
              | IncorrectCommand String -- некорректная команда 
              | UserNotInDB UserID      -- пользователь не найден в базе данных
              | IncorrectAccessRight    -- некорректные права пользователя
              | ErrorWritingFile        -- на всякий случай (?) - ошибка при записи в файл

instance Show MyError where
  show :: MyError -> String
  show (EmptyFile fileName)       = "The file " ++ fileName ++ " with UserDB is empty!"
  show IncorrectFile              = "Error with parsing file!"
  show (IncorrectCommand command) = "Incorrect command: " ++ command
  show (UserNotInDB user)         = "User " ++ show user ++ " not in database!"
  show IncorrectAccessRight       = "Incorrect access right!"
  show ErrorWritingFile           = "I have some problem with writing on file..."
