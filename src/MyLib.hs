{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib
  (
    padZero
  , encode
  , evenodd
  , average
  , gcContent
  , isReversePalindrom
  , meltingTemp
  , identity
  , fromListL
  , fromListR
  , nubOrd
  , buildQuery
  , AminoAcid(..)
  , toSymbol
  , aminoToStr
  , translate) where

import           Data.Char       (ord)
import           Data.Foldable   (foldl', toList)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Vector     as V

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
padZero str width = T.replicate (width - T.length str) "0" <> str

------------------------------------------------------------------------------------------------

-- 2. Шифр (0,5 балла)
--
-- Может пригодиться документация пакета vector: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html

cypher :: V.Vector Char
cypher = [ 'X', 'W', 'P', 'A', 'T', 'R', 'Q', 'I', 'U', 'C', 'B', 'D', 'F', 'H'
         , 'G', 'E', 'J', 'K', 'L', 'N', 'M', 'O', 'S', 'V', 'Y', 'Z']

-- >>> index 'A'
-- 0
-- >>> index 'H'
-- 7
index :: Char -> Int
index c = ord c - ord 'A'

-- Считаем что приходят только заглавные английские буквы
encode :: T.Text -> T.Text
encode = T.map (\c -> cypher V.! index c)

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

evenodd :: [a] -> ([a], [a])
evenodd = foldr splitList ([], []) . zip [0..]
    where
        splitList :: (Int, a) -> ([a], [a]) -> ([a], [a])
        splitList (idx, x) (leftList, rightList) | even idx  = (x : leftList, rightList)
                                                 | otherwise = (leftList, x : rightList)

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average vec | V.length vec > 0 = V.foldl' (+) 0 vec / fromIntegral (V.length vec)
            | otherwise        = error "Empty input string"

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

isSame :: Char -> Char -> Int
isSame c1 c2 | c1 == c2  = 1
             | otherwise = 0

isGC :: Char -> Int
isGC x = isSame 'G' x + isSame 'C' x

gcContent :: T.Text -> Double
gcContent text | T.length text > 0 = fromIntegral gcCount / fromIntegral (T.length text)
               | otherwise         = error "Empty input string"
    where
        gcCount :: Int
        gcCount = T.foldl' (\b x -> b + isGC x) 0 text

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
isReversePalindrom str = str == T.reverse complementSeq
    where
        complementMatrix :: M.Map Char Char
        complementMatrix = M.fromList [('A', 'T'), ('T', 'A'), ('C', 'G'), ('G', 'C')]

        complementSeq :: T.Text
        complementSeq = T.map (complementMatrix M.!) str

------------------------------------------------------------------------------------------------

-- 7. Температура плавления (0,75 балла)
--
-- Температура плавления короткой последовательности ДНК — важная характеристика для лабораторных экспериментов.
-- Её можно приближённо посчитать по формуле "4 * (число букв G или C) + 2 * (число букв A или T)" градусов.
--
-- Посчитать используя свёртку за один проход по последовательности.

meltingTemp :: T.Text -> Int
meltingTemp = T.foldl' (\b x -> b + nuclMeltingTemp x) 0
    where
        nuclMeltingTemp :: Char -> Int
        nuclMeltingTemp nucl = 2 * isGC nucl + 2  -- 4 * (isGC nucl) + 2 * (1 - isGC nucl)


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

countSame :: T.Text -> T.Text -> Int
countSame str1 = sum . map (uncurry isSame) . T.zip str1

identity :: T.Text -> T.Text -> Double
identity s1 s2 | T.length s1 == T.length s2 && T.length s1 > 0 =
                   fromIntegral (countSame s1 s2) / fromIntegral (T.length s1)
               | otherwise                               = error "Invalid identity call"

------------------------------------------------------------------------------------------------

-- 9. M.fromList (1 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: Ord k => [(k, v)] -> M.Map k v
fromListL = foldl' (flip . uncurry $ M.insert) M.empty

fromListR :: Ord k => [(k, v)] -> M.Map k v
fromListR = foldr (uncurry M.insert) M.empty

-- 1. foldl' имеет более оптимальное исполнение без риска stackoverflow,
--      при этом преимущества foldr в виде работы с бесконечными списками тут бесполезны
-- 2. M.insert будет перезаписывать значения, если происходит попытка вставить пару с существующим ключом,
--      из-за этого реализация с foldl' оставить только самую правую пару с одним ключом, fodlr - наоборот, левую

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
nubOrd = toList . foldl' (flip S.insert) S.empty

------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery = T.intercalate (T.singleton '&') . M.foldlWithKey' (\acc k v -> k <> T.singleton '=' <> v : acc) []

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

data AminoAcid = Ala | Cys | Asp | Glu | Phe | Gly | His | Ile | Lys | Leu | Met
               | Asn | Pyl | Pro | Gln | Arg | Ser | Thr | Sec | Val | Trp | Tyr
               | Ter
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
    toSymbol Ter = '*'

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

codonTable :: M.Map T.Text AminoAcid
codonTable = M.fromList $ concatMap (\(aa, codons) -> map (\c -> (T.pack c, aa)) codons) codonList
    where
        codonList :: [(AminoAcid, [String])]
        codonList = [
            (Phe, ["TTT", "TTC"])
            , (Leu, ["TTA", "TTG", "CTT", "CTC", "CTA", "CTG"])
            , (Ile, ["ATT", "ATC", "ATA"])
            , (Met, ["ATG"])
            , (Val, ["GTT", "GTC", "GTA", "GTG"])
            , (Ser, ["TCT", "TCC", "TCA", "TCG", "AGT", "AGC"])
            , (Pro, ["CCT", "CCC", "CCA", "CCG"])
            , (Thr, ["ACT", "ACC", "ACA", "ACG"])
            , (Ala, ["GCT", "GCC", "GCA", "GCG"])
            , (Tyr, ["TAT", "TAC"])
            , (Ter, ["TAA", "TAG", "TGA"])
            , (His, ["CAT", "CAC"])
            , (Gln, ["CAA", "CAG"])
            , (Asn, ["AAT", "AAC"])
            , (Lys, ["AAA", "AAG"])
            , (Asp, ["GAT", "GAC"])
            , (Glu, ["GAA", "GAG"])
            , (Cys, ["TGT", "TGC"])
            , (Trp, ["TGG"])
            , (Arg, ["CGT", "CGC", "CGA", "CGG", "AGA", "AGG"])
            , (Gly, ["GGT", "GGC", "GGA", "GGG"])]

translate :: T.Text -> [AminoAcid]
translate str | T.length str `mod` 3 == 0 = map (codonTable M.!) . T.chunksOf 3 $ str
              | otherwise                 = error "Incorrect DNA"
