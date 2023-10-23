{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE InstanceSigs #-}

{-
Компилятор выдает 2 ворнинга и они оба связаны с неопределением типов d 3 b 8 заданиях 
я попотался это побороть через прямое указание типов а-ля "что-то :: Double"
но не получается. Однако на работу программы это никак не влияет.
Все тесты и код выполняются корректно.
Hlint никаких рекомендаций не выдает.
-}

module MyLib where

import           Data.Char       (ord)
import           Data.Foldable   (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Data.Set        as S

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
padZero str width = T.replicate numZeros "0" <> str
    where
        numZeros = width - T.length str


------------------------------------------------------------------------------------------------

-- 2. Шифр (0,5 балла)
--
-- Может пригодиться документация пакета vector: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html

cypher :: V.Vector Char
cypher = ['X', 'W', 'P', 'Q', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'Z', 'C', 'V', 'B', 'N', 'M']

-- >>> index 'A'
-- 0
-- >>> index 'H'
-- 7
index :: Char -> Int
index c = ord c - ord 'A'

-- Считаем что приходят только заглавные английские буквы
encode :: T.Text -> T.Text
encode = T.map (\x -> cypher V.! index x)

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

evenodd :: [a] -> ([a], [a])
evenodd xs = foldr (\(i, x) (evens, odds) -> if even i then (x:evens, odds) else (evens, x:odds)) ([], []) (zip [0..] xs)



------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average vec
    | V.length vec > 0 = V.foldl' (+) 0 vec / fromIntegral (V.length vec)
    | otherwise        = 0

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

isGC :: Char -> Int
isGC x
    | x == 'G' || x == 'C' = 1
    | otherwise            = 0

gcContent :: T.Text -> Double
gcContent str 
    | T.length str > 0 = fromIntegral (T.foldl' (\acc c -> acc + isGC c) 0 str) / fromIntegral (T.length str)
    | otherwise    = 0

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

reverseComplementStr :: T.Text -> T.Text
reverseComplementStr = T.reverse . T.map complement
    where
        complement 'A' = 'T'
        complement 'T' = 'A'
        complement 'G' = 'C'
        complement 'C' = 'G'
        complement _   = error "WTF dude u have different non-human DNA"

isReversePalindrom :: T.Text -> Bool
isReversePalindrom str = str == reverseComplementStr str


------------------------------------------------------------------------------------------------

-- 7. Температура плавления (0,75 балла)
--
-- Температура плавления короткой последовательности ДНК — важная характеристика для лабораторных экспериментов.
-- Её можно приближённо посчитать по формуле "4 * (число букв G или C) + 2 * (число букв A или T)" градусов.
--
-- Посчитать используя свёртку за один проход по последовательности.

meltingTemp :: T.Text -> Int
meltingTemp = T.foldl' (\acc x -> acc + getTemp x) 0
  where
    getTemp 'G' = 4
    getTemp 'C' = 4
    getTemp 'A' = 2
    getTemp 'T' = 2
    getTemp _   = error "NONONONONONONONONONONONONONONONONONONO"


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
    | T.length str1 == T.length str2 && T.length str1 > 0 = identityCalculator str1 str2
    | otherwise = error "You can't calculate identity for sequences with different lengths"

identityCalculator :: T.Text -> T.Text -> Double
identityCalculator str1 str2 =
    let len = T.length str1
        matches = sum $ map (\(s1, s2) -> if s1 == s2 then 1 else 0) $ T.zip str1 str2
    in fromIntegral matches / fromIntegral len

------------------------------------------------------------------------------------------------

-- 9. M.fromList (1 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: Ord k => [(k, v)] -> M.Map k v
fromListL = foldl' (\acc (k, v) -> M.insert k v acc) M.empty
-- Начинаем с пустого листа и апдейтит его слева направо


fromListR :: Ord k => [(k, v)] -> M.Map k v
fromListR = foldr (\(k, v) acc -> M.insert k v acc) M.empty
-- Начинаем с пустого листа и апдейтит его справа налево
-- соответственно порядок будет разный итогового списка ключ-элемент

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
nubOrd = S.toList . foldl' (flip S.insert) S.empty
-- flib делает  (a -> b -> c) -> b -> a -> c, нужно чтоб порядок элементов в списке сохранить как в исходном

------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery parameters 
    | M.null parameters = "" 
    | otherwise = T.init $ M.foldlWithKey' (\acc key value -> acc <> key <> "=" <> value <> "&") "" parameters
-- T.init нужен чтобы отрубать лишний & в конце


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

data AminoAcid = Ala  -- Аланин
              | Arg   -- Аргинин
              | Asn   -- Аспарагин
              | Asp   -- Аспарагиновая кислота
              | Val   -- Валин
              | His   -- Гистидин
              | Gly   -- Глицин
              | Gln   -- Глутамин
              | Glu   -- Глутаминовая кислота
              | Ile   -- Изолейцин
              | Leu   -- Лейцин
              | Lys   -- Лизин
              | Met   -- Метионин
              | Pro   -- Пролин
              | Ser   -- Серин
              | Tyr   -- Тирозин
              | Thr   -- Треонин
              | Trp   -- Триптофан
              | Phe   -- Фенилаланин
              | Cys   -- Цистеин
              | Stop  -- Стоп-кодон
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
    toSymbol :: AminoAcid -> Char
    toSymbol Ala   = 'A'
    toSymbol Arg   = 'R'
    toSymbol Asn   = 'N'
    toSymbol Asp   = 'D'
    toSymbol Val   = 'V'
    toSymbol His   = 'H'
    toSymbol Gly   = 'G'
    toSymbol Gln   = 'Q'
    toSymbol Glu   = 'E'
    toSymbol Ile   = 'I'
    toSymbol Leu   = 'L'
    toSymbol Lys   = 'K'
    toSymbol Met   = 'M'
    toSymbol Pro   = 'P'
    toSymbol Ser   = 'S'
    toSymbol Tyr   = 'Y'
    toSymbol Thr   = 'T'
    toSymbol Trp   = 'W'
    toSymbol Phe   = 'F'
    toSymbol Cys   = 'C'
    toSymbol Stop  = '*'

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
    [ ("TTT", Phe), ("TTC", Phe), ("TTA", Leu),  ("TTG", Leu)
    , ("TCT", Ser), ("TCC", Ser), ("TCA", Ser),  ("TCG", Ser)
    , ("TAT", Tyr), ("TAC", Tyr), ("TAA", Stop), ("TAG", Stop)
    , ("TGT", Cys), ("TGC", Cys), ("TGA", Stop), ("TGG", Trp)
    , ("CTT", Leu), ("CTC", Leu), ("CTA", Leu),  ("CTG", Leu)
    , ("CCT", Pro), ("CCC", Pro), ("CCA", Pro),  ("CCG", Pro)
    , ("CAT", His), ("CAC", His), ("CAA", Gln),  ("CAG", Gln)
    , ("CGT", Arg), ("CGC", Arg), ("CGA", Arg),  ("CGG", Arg)
    , ("ATT", Ile), ("ATC", Ile), ("ATA", Ile),  ("ATG", Met)
    , ("ACT", Thr), ("ACC", Thr), ("ACA", Thr),  ("ACG", Thr)
    , ("AAT", Asn), ("AAC", Asn), ("AAA", Lys),  ("AAG", Lys)
    , ("AGT", Ser), ("AGC", Ser), ("AGA", Arg),  ("AGG", Arg)
    , ("GTT", Val), ("GTC", Val), ("GTA", Val),  ("GTG", Val)
    , ("GCT", Ala), ("GCC", Ala), ("GCA", Ala),  ("GCG", Ala)
    , ("GAT", Asp), ("GAC", Asp), ("GAA", Glu),  ("GAG", Glu)
    , ("GGT", Gly), ("GGC", Gly), ("GGA", Gly),  ("GGG", Gly)
    ]

translate :: T.Text -> [AminoAcid]
translate str
    | T.length str `mod` 3 /= 0 = error "String must be mod 3 length"
    | otherwise = translateCodons (T.chunksOf 3 str)
    where
        translateCodons [] = []
        translateCodons (codon:rest) = case M.lookup codon codonTable of
            Just amino -> amino : translateCodons rest
            Nothing    -> error ("Unknown codon: " ++ T.unpack codon)
