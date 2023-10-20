{- cabal:
build-depends: base
-}

import Prelude hiding (map, filter)


-- Стандартный map
map f [] = []
map f (x:xs) = f x : map f xs

-- Стандартный filter
filter f [] = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs

-- Как будет выглядеть композиция?
h f g = map f . filter g

-- А руками?
h' f g [] = []
h' f g (x:xs) = if g x then f x : h f g xs else h f g xs

-- Определим функции через foldr

mapF f xs = foldr (\x acc -> f x : acc) [] xs

filterF f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

-- <>
appendF :: [a] -> [a] -> [a]
appendF xs ys = foldr (\x acc -> x : acc) ys xs

-- Хотим чтобы "map g . filter g" был без промежуточного списка

-- Для демонстрации
a = undefined
b = undefined
c = undefined
d = undefined

-- Одно представление списка
lst = [a, b, c, d]
-- Эквивалентное
lst' = a : b : c : d : []

-- "Обобщим" : и []
lst'' cons nil = a `cons` (b `cons` (c `cons` (d `cons` nil)))

-- Как получить обратно список lst
foo = lst'' (:) []

-- Введём функци build, тогда "foo = build lst''"
build f = f (:) []

-- Теперь обобщим mapF, filterF через build

-- mapF f xs = foldr (\x acc -> f x : acc) [] xs
mapFB f xs = build $ \cons nil -> foldr (\x acc -> f x `cons` acc) nil xs

filterFB f xs = build $ \cons nil -> foldr (\x acc -> if f x then x `cons` acc else acc) nil xs

-- Распишем композицию mapFB f (filterFB g xs)
mapFilterFB :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilterFB f g xs =
  build $ \cons nil ->
    foldr (\x acc -> f x `cons` acc) nil
    $ build $ \cons1 nil1 -> foldr (\x acc -> if g x then x `cons1` acc else acc) nil1 xs

-- foldr/build rule, short cut fusion -- оптимизация позволяет устранить промежуточные списки
-- foldr k z (build g) = g k z

-- Выделим k, z и g в mapFilterFB
-- k = (\x acc -> f x `cons` acc)
-- z = nil
-- g = \cons1 nil1 -> foldr (\x1 acc1 -> if g x1 then x1 `cons1` acc1 else acc1) nil1

-- Применяем foldr/build
--g k z
--  = foldr (\x1 acc1 -> if g x1 then x1 `k` acc1 else acc1) z
--  -- Подставляем k
--  = foldr (\x1 acc1 -> if g x1 then (\x acc -> f x `cons` acc) x1 acc1 else acc1) z
--  -- Раскрываем лямбду
--  = foldr (\x1 acc1 -> if g x1 then f x1 `cons` acc1 else acc1) z

-- Подставляем в mapFilterFB
mapFilterFB' f g =
  build $ \cons nil ->
    foldr (\x1 acc1 -> if g x1 then f x1 `cons` acc1 else acc1) nil
-- Теперь нет промежуточного списка

-- Иллюстрация foldr/build
-- g = \c n -> "0" `c` "1" `c` n
-- build g = "0" : "1" : []
-- foldr k z (build g) = k "0" (foldr k z ("1" : [])) = .... = "0" `k` "1" `k` z
