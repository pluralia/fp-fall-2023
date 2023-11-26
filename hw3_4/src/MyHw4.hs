-- pragma ->
{-# LANGUAGE OverloadedStrings #-}

module MyHw4 where

{- cabal:
build-depends: base, containers, text, vector
-}

import Data.ByteString (replicate)
import Data.ByteString qualified as L
import Data.Char (ord)
import Data.Foldable (foldl', foldr, toList)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.TypeError qualified as T

-- К каждой задаче приведите хотя бы 1 или 2 тестовых примера.
-- Подсказка: Text и Vector можно конкатенировать с помощью оператора '<>'.
--
-- Старайтесь чтобы код был читаемым, используете понятные имена переменных, блоки where и
-- красивое форматирование.
--
-- Условие "за один проход" означает, что сложность итого алгоритма должна быть не больше O(n).
--
-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)
--
-- Многие из заданий напоминают то, что мы регулярно делаем в Biocad.

------------------------------------------------------------------------------------------------

-- 1. Дополнение нулями слева (0,5 балла)
--
-- Готовую функцию из пакета text использовать нельзя

str1 :: T.Text
str1 = "hh"

-- T.pack :: String -> T.Text
-- T.unpack :: T.Text -> String

padZero :: T.Text -> Int -> T.Text
padZero str width = T.append (T.replicate numZeros (T.pack "0")) str
  where
    numZeros = width - T.length str

-- padZero :: String -> Int -> String
-- padZero str width = replicate numZeros '0' <> str
--   where
--     numZeros = width - length str

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

isEven :: Int -> Bool
isEven x = if x `mod` 2 == 0 then True else False

evenodd :: [a] -> ([a], [a])
evenodd = foldl (\x acc -> ) ([], [])
  where
    helper x (evens, odds)
      | isEven x = (x : evens, odds)
      | otherwise = (evens, x : odds)

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average vec = undefined

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

gcContent :: T.Text -> Double
gcContent str = undefined

------------------------------------------------------------------------------------------------

-- 9. M.fromList (0,75 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: (Ord k) => [(k, v)] -> M.Map k v
fromListL lst = undefined

fromListR :: (Ord k) => [(k, v)] -> M.Map k v
fromListR lst = undefined

------------------------------------------------------------------------------------------------

-- 10. Уникальные элементы (0,5 балла)
--
-- Оставить только уникальные элементы из списка. Порядок может измениться.
--
-- Для решения этой задачи можно использовать Map или тип Set из пакета containers:
-- https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html
--
-- Решение должно использовать свёртку по входному списку в один проход. Использовать fromList нельзя.

nubOrd :: (Ord a) => [a] -> [a]
nubOrd xs = undefined

------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery parameters = undefined

------------------------------------------------------------------------------------------------
