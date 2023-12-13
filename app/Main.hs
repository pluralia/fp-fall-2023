{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Hashable (Hashable, hash)
import Control.Applicative (Alternative (..))
import Parser
import System.Directory (doesPathExist)

-- Продвинутый KVS (3 балла)

-- Создайте тип данных FileSystemKVS и сделайте его представителем класса типов KVS.

-- FileSystemKVS должен хранить объекты на диске в установленной директории.
-- Имя каждого объекта должно кодироваться укникальным хэшем (смотрите заготовку).

-- Также выполните все задания, помеченные в файле меткой TODO.


-- | В Haskell есть возможность указать переменные типа, которые никак не будут использоваться в его реализации.
--
--   Тут это пригодится: объекты хранятся на диске, но мы всё равно хотим предоставлять
--   информацию про их типы компилятору.
--
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
    toS :: Int -> String
    toS   = show
    fromS :: String -> Int
    fromS = read

-- TODO: сделайте 'String' представителем 'Serializable'
instance Serializable String where
    toS :: String -> String
    toS   = id
    fromS :: String -> String
    fromS = id

data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show :: Show a => Tree a -> String
    show Leaf                            = "L"
    show (Node val leftChild rightChild) = "N" ++ show val ++
        "(" ++ show  leftChild ++ ")" ++ "(" ++ show  rightChild ++ ")"

deserializeTree :: (Serializable a) => String -> Maybe (Tree a)
deserializeTree str = fst <$> runParser parserTree str
    where
        -- parserTree :: Parser (Tree a)
        parserTree = parserNode <|> parserLeaf
        -- parserNode :: Parser (Tree a)
        parserNode = Node <$ satisfyP (=='N')
                    <*> (fromS <$> some (satisfyP (/='(')))
                    <* satisfyP (=='(')
                    <*> parserTree
                    <* satisfyP (==')')
                    <* satisfyP (=='(')
                    <*> parserTree
                    <* satisfyP (==')')
        -- parserLeaf :: Parser (Tree a)
        parserLeaf = Leaf <$ satisfyP (=='L')

-- TODO: сделайте 'Tree a' представителем 'Serializable'
instance (Serializable a, Show a) => Serializable (Tree a) where
    toS :: (Serializable a, Show a) => Tree a -> String
    toS = show
    fromS :: (Serializable a, Show a) => String -> Tree a
    fromS strTree = case deserializeTree strTree of
        Just tree   -> tree
        Nothing     -> error "Uncorrect tree!"

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

    get :: (Hashable k, Serializable v) =>
            FileSystemKVS k v -> Key (FileSystemKVS k v) -> IO (Maybe (Value (FileSystemKVS k v)))
    get (FileSystemKVS path) key = do
        let filePath = path ++ "/" ++ (show . hash $ key)
        existFile <- doesPathExist filePath
        if existFile
            then do
                val <- readFile filePath
                pure . Just . fromS $ val
            else pure Nothing
    put :: (Hashable k, Serializable v) =>
            FileSystemKVS k v -> Key (FileSystemKVS k v) -> Value (FileSystemKVS k v) -> IO ()
    put (FileSystemKVS path) key val = do
        let filePath = path ++ "/" ++ (show . hash $ key)
        writeFile filePath (toS val)

main :: IO ()
main = do
    let path = "./app/"
        fs = createFileSystemKVS path :: FileSystemKVS [Char] [Char]
    put fs "key" "very small test :)"
    val <- get fs "key"
    maybe (pure ()) putStrLn val
