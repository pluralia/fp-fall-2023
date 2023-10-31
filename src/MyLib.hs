{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MyLib where

import           Data.Char       (ord)
import           Data.Foldable   (foldl')
import qualified Data.Map        as M
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Data.Set        as Set

-- К каждой задаче приведите хотя бы 1 или 2 тестовых примера.
-- Подсказка: Text и Vector можно конкатенировать с помощью оператора '<>'.
--
-- Старайтесь чтобы код был читаемым, используете понятные имена переменных, блоки where и
-- красивое форматирование.
--
-- Условие "за один проход" означает, что сложность итого алгоритма должна быть не больше O(n).
--
-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +2 баллов к стандартным 10)
--
-- Многие из заданий напоминают то, что мы регулярно делаем в Biocad.

------------------------------------------------------------------------------------------------

-- 1. Дополнение нулями слева (0,5 балла)
--
-- Готовую функцию из пакета text использовать нельзя

padZero :: T.Text -> Int -> T.Text
padZero str width = strZeros <> str
    where
        strZeros :: T.Text
        strZeros = T.replicate (width - T.length str) $ T.singleton '0'

------------------------------------------------------------------------------------------------

-- 2. Шифр (0,5 балла)
--
-- Может пригодиться документация пакета vector: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html

cypher :: V.Vector Char
cypher = ['X', 'W', 'P', 'Q', 'E', 'R', 'T', 'Y', 'U', 'I',
          'O', 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L',
          'Z', 'C', 'V', 'B', 'N', 'M']                     -- произвольно дополнить до 26 символов

-- A = X, C = P, G = T, T = L

-- >>> index 'A'
-- 0
-- >>> index 'H'
-- 7
index :: Char -> Int
index c = ord c - ord 'A'   -- из Data.Char

-- Считаем что приходят только заглавные английские буквы
encode :: T.Text -> T.Text
encode = T.map (\x -> cypher V.! index x)

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

evenodd :: [a] -> ([a], [a])
evenodd xs = (subArray False (zip [0..] xs), subArray True (zip [0..] xs))
    where
        subArray :: Bool -> [(Int, a)] -> [a]
        subArray oddFlag = map snd . filter (\(i, _) -> oddFlag /= even i)

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average xs | V.length xs == 0 = error "Vector is empty!"
           | otherwise        = foldl' (+) 0 xs / fromIntegral (V.length xs)

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

gcContent :: T.Text -> Double
gcContent str | T.length str == 0 = error "Sequence is empty!"
              | otherwise         = T.foldl' (\acc x ->
                    if x == 'G' || x == 'C' then acc + 1 else acc) 0 str / fromIntegral (T.length str)

------------------------------------------------------------------------------------------------

-- 6. Обратный палиндром (0,75 балла)
--
-- Проверить, что последовательность ДНК совпадает со своей обратно-комплементарной
--
-- Последовательность состоит из букв A, T, G, C.
-- Обратно-комплементарная последовательность записывается в обратном порядке с заменами A<->T, G<->C.
--
-- Пример: GCTGCAA обратно-комплементарна TTGCAGC
--
-- Для решения этой задачи потребуются найти нужные функции в Hoogle или документации пакета text:
-- https://hackage.haskell.org/package/text-2.1/docs/Data-Text.html


isReversePalindrom :: T.Text -> Bool
isReversePalindrom str = helper (T.length str) 0
    where
        dict :: M.Map Char Char
        dict = M.fromList [('A', 'T'), ('T', 'A'), ('C', 'G'), ('G', 'C')]

        helper :: Int -> Int -> Bool
        helper n ind | n == ind  = True
                     | otherwise =
                        (str `T.index` ind == dict M.! (str `T.index` (n - 1 - ind))) &&
                            helper n (ind + 1)

------------------------------------------------------------------------------------------------

-- 7. Температура плавления (0,75 балла)
--
-- Температура плавления короткой последовательности ДНК — важная характеристика для лабораторных экспериментов.
-- Её можно приближённо посчитать по формуле "4 * (число букв G или C) + 2 * (число букв A или T)" градусов.
--
-- Посчитать используя свёртку за один проход по последовательности.

meltingTemp :: T.Text -> Int
meltingTemp =
    T.foldl' (\acc x -> if x == 'C' || x == 'G' then acc + 4 else acc + 2) 0

------------------------------------------------------------------------------------------------

-- 8. Identity (0,75 балла)
--
-- Identity — мера похожести двух последовательностей одной длины, вычисляется как (1 - hamming) / length,
-- где hamming — "расстояние Хэмминга" между последовательностями, то есть число позиций с отличающимися буквами.
--
-- Например, для последовательностей AGCCAGT и AGTCACC расстояние Хэмминга равно 3, а identity — 4/7.
--
-- Посчитать identity двух последовательностей за один проход. Если на вход поданы последовательности
-- разной длины, выдать ошибку 

identity :: T.Text -> T.Text -> Double
identity str1 str2 | T.length str1 /= T.length str2 = error "Different sequences length!"
                   | T.length str1 == 0             = error "Sequences are empty!"
                   | otherwise                      =
                        1 - (hammingDistance (T.zip str1 str2) / fromIntegral (T.length str1))
    where
        hammingDistance :: [(Char, Char)] -> Double
        hammingDistance = foldl' (\acc x -> if uncurry (/=) x then acc + 1 else acc) 0.0

------------------------------------------------------------------------------------------------

-- 9. M.fromList (1 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: Ord k => [(k, v)] -> M.Map k v
fromListL = foldl' (flip (uncurry M.insert)) M.empty

fromListR :: Ord k => [(k, v)] -> M.Map k v
fromListR = foldr (uncurry M.insert) M.empty

-- Отличие в необходимости использования flip, так как аргументы в foldl' и foldr 
-- передаются в разном порядке. Из-за того, что аргументы передаются в разном порядке, 
-- в случае повтора tupl'ов с парой (k, v) для foldr сохранится самый последний (правый) вариант, 
-- а для foldl самый первый (левый).

------------------------------------------------------------------------------------------------

-- 10. Уникальные элементы (0,5 балла)
--
-- Оставить только уникальные элементы из списка. Порядок может измениться.
--
-- Для решения этой задачи можно использовать Map или тип Set из пакета containers:
-- https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html
--
-- Решение должно использовать свёртку по входному списку в один проход. Использовать fromList нельзя.

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . foldl' (flip Set.insert) Set.empty

-- Так как пустой set создаётся за константу, а следующий элемент добавляется за логарифм, 
-- значит суммарно асимптотика будет O(n * log(n)).
-- Не знаю, можно ли решить за линию (возможно с использованием group, 
-- но асимптотику этой функции я не нашла).

------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery = T.intercalate (T.singleton '&') . map (\(k, v) -> k <> T.singleton '=' <> v) . M.toList

------------------------------------------------------------------------------------------------

-- 12. Биология (2,25 балла)

-- a. Аминокислоты (0,25 балла)
--
-- Определите тип AminoAcid, перечисляющий аминокислоты. Конструкторы должны соответствовать 3-буквенным
-- обозначениям аминокислот. Список можно взять здесь: https://en.wikipedia.org/wiki/Proteinogenic_amino_acid#Chemical_properties.
--
-- Добавьте к аминокислотам из списка ещё один конструктор для стоп-кодона.
--
-- Выведите для этого типа инстансы Eq, Show и Ord автоматически.

data AminoAcid = Ala | Cys | Asp | Glu | Phe | Gly | His | Ile | Lys | Leu | Met |
                 Asn | Pyl | Pro | Gln | Arg | Ser | Thr | Sec | Val | Trp | Tyr | Stp
    deriving (Show, Eq, Ord)

------------------------------------------

-- b. Однобуквенный код (0,75 балла)
--
-- Определите класс `ToSymbol a` с единственным методом, превращающим `a` в `Char`. Определите инстанс этого
-- класса для типа AminoAcid, преобразующий аминокислоты в однобуквенный код (A, C, D, ... — см. таблицу в предыдущем пункте).
-- Стоп-кодон превращается в символ "*".

class ToSymbol a where
    transformation :: a -> Char

instance ToSymbol AminoAcid where
    transformation :: AminoAcid -> Char
    transformation code | code == Ala = 'A'
                        | code == Cys = 'C'
                        | code == Asp = 'D'
                        | code == Glu = 'E'
                        | code == Phe = 'F'
                        | code == Gly = 'G'
                        | code == His = 'H'
                        | code == Ile = 'I'
                        | code == Lys = 'K'
                        | code == Leu = 'L'
                        | code == Met = 'M'
                        | code == Asn = 'N'
                        | code == Pyl = 'O'
                        | code == Pro = 'P'
                        | code == Gln = 'Q'
                        | code == Arg = 'R'
                        | code == Ser = 'S'
                        | code == Thr = 'T'
                        | code == Sec = 'U'
                        | code == Val = 'V'
                        | code == Trp = 'W'
                        | code == Tyr = 'Y'
                        | code == Stp = '*'

------------------------------------------

-- c. Строка (0,5 балла)
--
-- Определите функцию, превращающую список аминокислот в строку из однобуквенного кода.

aminoToStr :: [AminoAcid] -> T.Text
aminoToStr = T.intercalate T.empty . map (T.singleton . transformation)

------------------------------------------

-- d. Трансляция (0,75 балла)
--
-- Вам дана строка из нуклеотидов (буквы A, T, G, C) длины кратной 3. Надо превратить её в аминокислотную
-- строку.
--
-- Используйте стандартную таблицу кодонов: https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables#Standard_DNA_codon_table.
--
-- Храните соответствие кодонов и аминокислот в M.Map.
--
-- Если длина строки не кратна 3, вернуть ошибку.

dictAA :: M.Map T.Text AminoAcid
dictAA = M.fromList [("TTT", Phe), ("TTC", Phe),
                     ("TTA", Leu), ("TTG", Leu), ("CTT", Leu), ("CTC", Leu), ("CTA", Leu), ("CTG", Leu),
                     ("ATT", Ile), ("ATC", Ile), ("ATA", Ile),
                     ("ATG", Met),
                     ("GTT", Val), ("GTC", Val), ("GTA", Val), ("GTG", Val),
                     ("TCT", Ser), ("TCC", Ser), ("TCA", Ser), ("TCG", Ser),
                     ("CCT", Pro), ("CCC", Pro), ("CCA", Pro), ("CCG", Pro),
                     ("ACT", Thr), ("ACC", Thr), ("ACA", Thr), ("ACG", Thr),
                     ("GCT", Ala), ("GCC", Ala), ("GCA", Ala), ("GCG", Ala),
                     ("TAT", Tyr), ("TAC", Tyr),
                     ("TAA", Stp), ("TAG", Stp), ("TGA", Stp),
                     ("CAT", His), ("CAC", His),
                     ("CAA", Gln), ("CAG", Gln),
                     ("AAT", Asn), ("AAC", Asn),
                     ("AAA", Lys), ("AAG", Lys),
                     ("GAT", Asp), ("GAC", Asp),
                     ("GAA", Glu), ("GAG", Glu),
                     ("TGT", Cys), ("TGC", Cys),
                     ("TGG", Trp),
                     ("CGT", Arg), ("CGC", Arg), ("CGA", Arg), ("CGG", Arg), ("AGA", Arg), ("AGC", Arg),
                     ("AGT", Ser), ("AGC", Ser),
                     ("GGT", Gly), ("GGC", Gly), ("GGA", Gly), ("GGG", Gly)]

translate :: T.Text -> [AminoAcid]
translate s | T.length s `mod` 3 /= 0 = error "Uncorrect length sequence!"
            | otherwise                 = map (dictAA M.!) (T.chunksOf 3 s)
