myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n xs
    | n<=0 = xs
    | otherwise = myDrop (n-1) xs


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myReplicate :: Int -> a -> [a]
myReplicate n x
    | n <= 0 = []
    | otherwise = x : myReplicate (n-1) x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x



