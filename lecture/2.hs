map' f [] = []
map' f (x : xs) = f x : map' f xs

filter' f [] = []
filter' f (x : xs) = if f x then x : filter f xs else filter f xs

-- h = map f . filter g
-- h = T.map f . T.filter g

h f q [] = []
h f g (x : xs) = if f x then f x : h f g xs else h f g xs

-- with foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> [b]
mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr (\x acc -> f x : acc) []

filterF :: (a -> Bool) -> [a] -> [a]
filterF f = foldr (\x acc -> if f x then x : acc else  acc) []

-- foldr func acc -> acc
appendF :: [a] -> [a] -> [a]
appendF xs ys = foldr (:) ys xs

a = 1
b = 2
c = 3
d = 4
lst = [a, b, c, d]
lst' = a : b : c : d : []

lst'' const nil = a `const` b `const` c `const` d `const` nil

-- foo = (... lst'' ...) -- equals to list
build f = f (:) []

mapFB f = build $ \cons nil -> const : nil