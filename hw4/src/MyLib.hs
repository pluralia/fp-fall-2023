module MyLib where
{- cabal:
build-depends: base, containers, text, vector
-}

{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Char       (ord)
import           Data.Foldable   (foldl')
import qualified Data.Map.Strict as M
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
padZero str width = T.append (T.replicate (width - T.length str) (T.pack "0")) str

------------------------------------------------------------------------------------------------

-- 2. Шифр (0,5 балла)
--
-- Может пригодиться документация пакета vector: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html

cypher :: V.Vector Char
cypher = V.fromList['Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'Z', 'X', 'C', 'V', 'B', 'N', 'M'] -- произвольно дополнить до 26 символов

-- >>> index 'A'
-- 0
-- >>> index 'H'
-- 7
index :: Char -> Int
index c = ord c - ord 'A'

-- Считаем что приходят только заглавные английские буквы
encode :: T.Text -> T.Text
encode = T.map (\ c -> cypher V.! index c)

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

evenodd :: [a] -> ([a], [a])
evenodd = foldr (\ x (ys, zs) -> (zs, x : ys)) ([], [])

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average vec
    | V.null vec = 0.0
    |otherwise = 
        let sumAndCount = V.foldl' step (0, 0) vec
            step :: (Double, Int) -> Double -> (Double, Int)
            step (s, l) x = (s + x, l + 1)
            (sum', len) = fmap fromIntegral sumAndCount
        in sum' / len

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

gcContent :: T.Text -> Double
gcContent str = 
    let (gcCount, totalCount) = T.foldl' step (0, 0) str
        step :: (Int, Int) -> Char -> (Int, Int)
        step (gc, count') c 
            | c == 'G' || c == 'C' = (gc + 1, count' + 1)
            | c == 'A' || c == 'T' = (gc, count' + 1)
            | otherwise = (gc, count')
    in if totalCount == 0 then 0 else fromIntegral gcCount / fromIntegral totalCount

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
-- !!!!!!!!!!!!!!


isReversePalindrom :: T.Text -> Bool
isReversePalindrom dna = T.reverse (T.map complement dna) == dna
    where
        complement :: Char -> Char
        complement 'A' = 'T'
        complement 'T' = 'A'
        complement 'G' = 'C'
        complement 'C' = 'G'
        complement  _  = error "Incorrect input"


------------------------------------------------------------------------------------------------

-- 7. Температура плавления (0,75 балла)
--
-- Температура плавления короткой последовательности ДНК — важная характеристика для лабораторных экспериментов.
-- Её можно приближённо посчитать по формуле "4 * (число букв G или C) + 2 * (число букв A или T)" градусов.
--
-- Посчитать используя свёртку за один проход по последовательности.

meltingTemp :: T.Text -> Int
meltingTemp = T.foldl' (\ acc c -> acc + temp c) 0
    where
        temp 'G' = 4
        temp 'C' = 4
        temp 'A' = 2
        temp 'T' = 2
        temp _   = 0

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
identity str1 str2
    | T.length str1 /= T.length str2 = -1.0 -- вместо ошибки возращаем -1 так как в типы указан был только Double
    | otherwise                      = 1 - hamming / len
    where
        len = fromIntegral $ T.length str1
        hamming = fromIntegral $ length $ filter not $ zipWith (==) (T.unpack str1) (T.unpack str2)
------------------------------------------------------------------------------------------------

-- 9. M.fromList (1 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: Ord k => [(k, v)] -> M.Map k v
fromListL = foldl' (\ acc (k, v) -> M.insert k v acc) M.empty

fromListR :: Ord k => [(k, v)] -> M.Map k v
fromListR = foldr (\ (k, v) acc -> M.insert k v acc) M.empty

-- Отличаться будет поведение, когда k одинаковое. Потому что в зависимости от свертки, смотря в какую сторону мы идем, то значение и остнется
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
nubOrd = reverse . uniqueElements
    where
        uniqueElements :: Ord a => [a] -> [a]
        uniqueElements = snd . foldl processElement (Set.empty, [])

        processElement :: Ord a => (Set.Set a, [a]) -> a -> (Set.Set a, [a])
        processElement (set, list) element
            | Set.member element set = (set, list)
            | otherwise              = (Set.insert element set, element:list)
------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery parameters = T.drop 1 $ M.foldlWithKey' appendKeyValue (T.pack "") parameters
    where
        appendKeyValue acc k v = acc `T.append` T.pack "&" `T.append` k `T.append` T.pack "=" `T.append` v

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

data AminoAcid = Ala | Cys | Asp | Glu | Phe | Gly | His | Ile 
               | Lys | Leu | Met | Asn | Pyl | Pro | Gln | Arg 
               | Ser | Thr | Sec | Val | Trp | Tyr
               | StopCodon
               deriving (Eq, Show, Ord)


------------------------------------------

-- b. Однобуквенный код (0,75 балла)
--
-- Определите класс `ToSymbol a` с единственным методом, превращающим `a` в `Char`. Определите инстанс этого
-- класса для типа AminoAcid, преобразующий аминокислоты в однобуквенный код (A, C, D, ... — см. таблицу в предыдущем пункте).
-- Стоп-кодон превращается в символ "*".

class ToSymbol a where
  toSymbol :: a -> Char

instance ToSymbol AminoAcid where
    toSymbol Ala = 'A'
    toSymbol Cys = 'C'
    toSymbol Asp = 'D'
    toSymbol Glu = 'E'
    toSymbol Phe = 'F'
    toSymbol Gly = 'G'
    toSymbol His = 'H'
    toSymbol Ile = 'I'
    toSymbol Lys = 'K'
    toSymbol Leu = 'L'
    toSymbol Met = 'M'
    toSymbol Asn = 'N'
    toSymbol Pyl = 'O'
    toSymbol Pro = 'P'
    toSymbol Gln = 'Q'
    toSymbol Arg = 'R'
    toSymbol Ser = 'S'
    toSymbol Thr = 'T'
    toSymbol Sec = 'U'
    toSymbol Val = 'V'
    toSymbol Trp = 'W'
    toSymbol Tyr = 'Y'
    toSymbol StopCodon= '*'
------------------------------------------

-- c. Строка (0,5 балла)
--
-- Определите функцию, превращающую список аминокислот в строку из однобуквенного кода.

aminoToStr :: [AminoAcid] -> T.Text
aminoToStr = T.pack . map toSymbol

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

type Codon = T.Text

codonTable :: M.Map Codon AminoAcid
codonTable = M.fromList 
  [ (T.pack "GCT", Ala), (T.pack "GCC", Ala), (T.pack "GCA", Ala), (T.pack "GCG", Ala)
  , (T.pack "TGT", Cys), (T.pack "TGC", Cys)
  , (T.pack "GAT", Asp), (T.pack "GAC", Asp)
  , (T.pack "GAA", Glu), (T.pack "GAG", Glu)
  , (T.pack "TTT", Phe), (T.pack "TTC", Phe)
  , (T.pack "GGT", Gly), (T.pack "GGC", Gly), (T.pack "GGA", Gly), (T.pack "GGG", Gly)
  , (T.pack "CAT", His), (T.pack "CAC", His)
  , (T.pack "ATT", Ile), (T.pack "ATC", Ile), (T.pack "ATA", Ile)
  , (T.pack "AAA", Lys), (T.pack "AAG", Lys)
  , (T.pack "TTA", Leu), (T.pack "TTG", Leu), (T.pack "CTT", Leu), (T.pack "CTC", Leu), 
    (T.pack "CTA", Leu), (T.pack "CTG", Leu)
  , (T.pack "ATG", Met)
  , (T.pack "AAT", Asn), (T.pack "AAC", Asn)
  , (T.pack "CCT", Pro), (T.pack "CCC", Pro), (T.pack "CCA", Pro), (T.pack "CCG", Pro)
  , (T.pack "CAA", Gln), (T.pack "CAG", Gln)
  , (T.pack "CGT", Arg), (T.pack "CGC", Arg), (T.pack "CGA", Arg), (T.pack "CGG", Arg), (T.pack "AGA", Arg), (T.pack "AGG", Arg)
  , (T.pack "TCT", Ser), (T.pack "TCC", Ser), (T.pack "TCA", Ser), (T.pack "TCG", Ser), (T.pack "AGT", Ser) ,(T.pack "AGC", Ser)
  ,(T.pack "ACT", Thr), (T.pack "ACC", Thr), (T.pack "ACA", Thr), (T.pack "ACG", Thr)
  ,(T.pack "GTT", Val), (T.pack "GTC", Val), (T.pack "GTA", Val), (T.pack "GTG", Val)
  ,(T.pack "TGG", Trp)
   ,(T.pack "TAT", Tyr), (T.pack "TAC", Tyr)
   -- Stop codons
   ,(T.pack "TAG", StopCodon), (T.pack "TAA", StopCodon), (T.pack "TAGA", StopCodon)
   ]


translate :: T.Text -> Either String [AminoAcid]
translate dna
    |T.length dna `mod` 3 /= 0 = Left "Input is not correct"
    | otherwise = Right (go (T.unpack dna))
    where
        go [] = []
        go xs = case M.lookup (T.pack (take 3 xs)) codonTable of
            Just a -> a : go (drop 3 xs)
            Nothing -> []