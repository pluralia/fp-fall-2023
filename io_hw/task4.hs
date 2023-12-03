{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}

module Main where

-- Продвинутый KVS (3 балла)

-- Создайте тип данных FileSystemKVS и сделайте его представителем класса типов KVS.

-- FileSystemKVS должен хранить объекты на диске в установленной директории.
-- Имя каждого объекта должно кодироваться укникальным хэшем (смотрите заготовку).

-- Также выполните все задания, помеченные в файле меткой TODO.

import Data.Hashable (Hashable)

-- | В Haskell есть возможность указать переменные типа, которые никак не будут использоваться в его реализации.
--
--   Тут это пригодится: объекты хранятся на диске, но мы всё равно хотим предоставлять
--   информацию про их типы компилятору.
--
data FileSystemKVS k v = YourImplementation

-- TODO: реализуйте функцию createFileSystemKVS

-- | Создаёт объект 'FileSystemKVS', который сохраняет объекты в директорию @pathToDir@.
--
createFileSystemKVS :: FilePath -> FileSystemKVS k v
createFileSystemKVS pathToDir = undefined

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
    toS = undefined
    fromS = undefined

-- TODO: сделайте 'String' представителем 'Serializable'
instance Serializable String where
    toS = undefined
    fromS = undefined

data Tree a = Leaf | Node a (Tree a) (Tree a)

-- TODO: сделайте 'Tree a' представителем 'Serializable'
instance Serializable a => Serializable (Tree a) where
    toS = undefined
    fromS = undefined

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

    get = undefined
    put = undefined

main :: IO ()
main = undefined
