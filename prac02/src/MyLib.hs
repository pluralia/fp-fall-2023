module MyLib where

myInit' :: [a] -> Maybe [a]
myInit' []       = Nothing
myInit' [_]      = Just []
myInit' (x : xs) = Just (x : unpack (myInit xs))
  where
    unpack (Just value) = value
    unpack Nothing      = undefined -- impossible case

myInit :: [a] -> Maybe [a]
myInit []       = Nothing
myInit [_]      = Just []
myInit (x : xs) = fmap (x :) (myInit xs)

-- fmap :: (a -> b) -> Maybe a -> Maybe b
-- fmap :: ([a] -> [a]) -> Maybe [a] -> Maybe [a]

myInitTail :: [a] -> Maybe [a] 
myInitTail s = fmap reverse (go s (Just []))
  where
    go :: [a] -> Maybe [a] -> Maybe [a]
    go []       _   = Nothing
    go [_]      acc = acc
    go (x : xs) acc = go xs (fmap (x :) acc)

-- удаляет n первых элементов из списка
myDrop :: Int -> [a] -> [a]
myDrop 0     xs = xs
myDrop _     [] = []
myDrop count (_ : xs) = myDrop (count - 1) xs

-- берет 2 списка и соединяет их поэлементно; размер итогового списка равен размеру наименьшего списка
-- [1..3], [3..4] -> [(1, 3), (2, 4)]
myZip :: [a] -> [b] -> [(a, b)]
myZip []        _       = []
myZip _        []       = []
myZip (a : as) (b : bs) = (a,b) : myZip as bs

-- повторяет элемент списка n раз
myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1..n]]

-- меняет местами аргументы функции
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x

-- опционально для разминки
mySum :: Num a => [a] -> a
mySum = undefined

-- опционально для разминки
myProduct :: Num a => [a] -> a
myProduct = undefined

-- опционально для разминки
myConcat :: [[a]] -> [a]
myConcat = undefined

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc []       = acc
myFoldr f acc (x : xs) = f x (myFoldr f acc xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc []       = acc 
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs
