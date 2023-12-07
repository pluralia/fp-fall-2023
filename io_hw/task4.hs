{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}

module Main where

-- Продвинутый KVS (3 балла)

-- Создайте тип данных FileSystemKVS и сделайте его представителем класса типов KVS.

-- FileSystemKVS должен хранить объекты на диске в установленной директории.
-- Имя каждого объекта должно кодироваться укникальным хэшем (смотрите заготовку).

-- Также выполните все задания, помеченные в файле меткой TODO.

import Data.Hashable (Hashable, hash)

-- | В Haskell есть возможность указать переменные типа, которые никак не будут использоваться в его реализации.
--
--   Тут это пригодится: объекты хранятся на диске, но мы всё равно хотим предоставлять
--   информацию про их типы компилятору.
--
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

newtype FileSystemKVS k v = FileSystemKVS FilePath
-- TODO: реализуйте функцию createFileSystemKVS

-- | Создаёт объект 'FileSystemKVS', который сохраняет объекты в директорию @pathToDir@.
--
createFileSystemKVS :: FilePath -> FileSystemKVS k v
createFileSystemKVS = FileSystemKVS

-- | Класс типов, описывающий объекты, которые можно представить в виде строк.
--
class Serializable a where
    -- | Превращает объект в строку.
    --
    toS   :: a -> String

    -- | Парсит строку в объект.
    --
    fromS :: String -> a

    fromFile :: FilePath -> IO a
    fromFile = fmap fromS . readFile

    toFile :: FilePath -> a -> IO ()
    toFile file = writeFile file . toS

-- TODO: сделайте 'Int' представителем 'Serializable'
instance Serializable Int where
    toS = show
    fromS = read

-- TODO: сделайте 'String' представителем 'Serializable'
instance Serializable String where
    toS = id
    fromS = id

data Tree a = Leaf | Node a (Tree a) (Tree a)

-- TODO: сделайте 'Tree a' представителем 'Serializable'
instance Serializable a => Serializable (Tree a) where
    toS Leaf = "Leaf"
    toS (Node val left right) = "Node " ++ toS val ++ " (" ++ toS left ++ ") (" ++ toS right ++ ")"

    fromS "Leaf" = Leaf
    fromS str = case words str of
        ["Node", val, "(", left, ")", "(", right, ")"] ->
            Node (fromS val) (fromS left) (fromS right)
        _ -> error "Failed to parse Tree from string"

-- | KVS, допускающий вычисления в IO при работе с ключами и значениями.
--
--   Ключи в 'KVS'е являются 'Hashable' для того, чтобы можно было использовать
--   их в качестве уникальных имён файлов в системе.
--
--   Значения в 'KVS'е являются 'Serializable' для того, чтобы можно было
--   записывать их в файлы.
--  
class (Hashable (Key a), Serializable (Value a)) => KVS a where
    type Key a
    type Value a

    -- | Получает из @a@ значение по ключу @Key a@.
    --   Если по данному ключу нет значения, то возвращает @Nothing@.
    --
    get :: a -> Key a -> IO (Maybe (Value a))

    -- | Кладёт в @a@ по ключу @Key a@ значение @Value a@.
    --   Если значение по данному ключу уже есть, то перезаписывает его.
    --
    put :: a -> Key a -> Value a -> IO ()

instance (Hashable k, Serializable v) => KVS (FileSystemKVS k v) where
    type Key (FileSystemKVS k v) = k
    type Value (FileSystemKVS k v) = v

    get (FileSystemKVS dir) key = do
        let filePath = dir </> show (hash key)
        fileExists <- doesFileExist filePath
        if fileExists
            then do
                content <- fromFile filePath
                return $ Just content
            else
                return Nothing

    put (FileSystemKVS dir) key value = do
        let filePath = dir </> show (hash key)
        createDirectoryIfMissing True dir
        toFile filePath value

main :: IO ()
main = putStrLn "Hello, IO!" 
    
    --   do
    -- let fsKVS = createFileSystemKVS "data"

    -- put fsKVS ("key1" :: String) (42 :: Int)

    -- result <- get fsKVS ("key1" :: String)
    -- case result of
    --     Just value -> putStrLn $ "Value for key1: " ++ show (value :: Int)
    --     Nothing    -> putStrLn "Key1 not found"

    -- result2 <- get fsKVS ("nonexistent" :: String)
    -- case result2 of
    --     Just value -> putStrLn $ "Value for nonexistent key: " ++ show (value :: Int)
    --     Nothing    -> putStrLn "Nonexistent key not found" 
-- это вылетает с ошибкой на этапе компиляции



