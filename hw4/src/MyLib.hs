{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import           Debug.Trace
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
padZero str width = T.replicate (width - T.length str) "0" <> str

------------------------------------------------------------------------------------------------

-- 2. Шифр (0,5 балла)
--
-- Может пригодиться документация пакета vector: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html

cypher :: V.Vector Char
cypher = V.fromList ['X', 'W', 'P', 'C', 'M', 'Q', 'L', 'F', 'E', 
                     'J', 'K', 'N', 'O', 'R', 'S', 'T', 'U', 'V', 
                     'A', 'B', 'D', 'G', 'H', 'I', 'Y', 'Z'] -- произвольно дополнить до 26 символов

-- >>> index 'A'
-- 0
-- >>> index 'H'
-- 7
index :: Char -> Int
index c = ord c - ord 'A'

-- Считаем что приходят только заглавные английские буквы
encode :: T.Text -> T.Text
encode = T.map ((cypher V.!) . index)   

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

evenodd :: [a] -> ([a], [a])
evenodd = foldr helper ([], []) . zip [0..]
  where
    helper :: (Int, a) -> ([a], [a]) -> ([a], [a])
    helper (i, x) (evens, odds) = if even i
                                  then (x:evens, odds)
                                  else (evens, x:odds)

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average = uncurry (/) . foldl' (\(total, len) x -> (total + x, len + 1)) (0, 0) 

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

gcContent :: T.Text -> Maybe Double
-- pattern matching empty string  
gcContent "" = Nothing
gcContent s = Just $ uncurry (/) . T.foldl' helper (0, 0) $ s
  where
    helper :: (Double, Double) -> Char -> (Double, Double)
    helper (gc, len) x = if x == 'G' || x == 'C'
                         then (gc + 1, len + 1)
                         else (gc, len + 1)

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
isReversePalindrom str = (==) str . T.map complement . T.reverse $ str
  where 
    complement :: Char -> Char
    complement nuc = case M.lookup nuc complementsMap of
                       Just comp -> comp
                       Nothing   -> error "Invalid nucleotide"
    complementsMap :: M.Map Char Char
    complementsMap = M.fromList [('A', 'T'), ('T', 'A'), ('G', 'C'), ('C', 'G')]

------------------------------------------------------------------------------------------------

-- 7. Температура плавления (0,75 балла)
--
-- Температура плавления короткой последовательности ДНК — важная характеристика для лабораторных экспериментов.
-- Её можно приближённо посчитать по формуле "4 * (число букв G или C) + 2 * (число букв A или T)" градусов.
--
-- Посчитать используя свёртку за один проход по последовательности.

meltingTemp :: T.Text -> Int
meltingTemp = T.foldl' helper 0
  where
    helper :: Int -> Char -> Int
    helper temp x = if x == 'G' || x == 'C'
                    then temp + 4
                    else temp + 2

------------------------------------------------------------------------------------------------

-- 8. Identity (0,75 балла)
--
-- Identity — мера похожести двух последовательностей одной длины, вычисляется как 1 - hamming / length,
-- где hamming — "расстояние Хэмминга" между последовательностями, то есть число позиций с отличающимися буквами.
--
-- Например, для последовательностей AGCCAGT и AGTCACC расстояние Хэмминга равно 3, а identity — 4/7.
--
-- Посчитать identity двух последовательностей за один проход. Если на вход поданы последовательности
-- разной длины, выдать ошибку 

identity :: T.Text -> T.Text -> Double
identity str1 str2 = if T.length str1 /= T.length str2
                     then error "Sequences have different lengths"
                     else (-) 1 . uncurry (/) . foldl' helper (0, 0) $ T.zip str1 str2
  where
    helper :: (Double, Double) -> (Char, Char) -> (Double, Double)
    helper (hamming, len) (x, y) = if x == y
                                   then (hamming, len + 1)
                                   else (hamming + 1, len + 1)

------------------------------------------------------------------------------------------------

-- 9. M.fromList (1 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: Ord k => [(k, v)] -> M.Map k v
fromListL = foldl' (\mp (key, value) -> M.insert key value mp) M.empty


fromListR :: Ord k => [(k, v)] -> M.Map k v
fromListR = foldr (uncurry M.insert) M.empty


-- Отличие в порядке добавления элементов в Map. В первом случае элементы добавляются в порядке их следования в списке,
-- во втором — в обратном порядке. Таким образом, в случае совпадения ключей в первом случае (foldl') будет использовано значение,
-- которое было в списке последним, во втором (foldr) — первым.

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

------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery = T.intercalate "&" . M.foldlWithKey' helper []
  where
    helper :: [T.Text] -> T.Text -> T.Text -> [T.Text]
    helper acc key value = (key <> "=" <> value):acc

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

data AminoAcid = Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | 
                 Leu | Lys | Met | Phe | Pro | Pyl | Sec | Ser | Thr | Trp | 
                 Tyr | Val | Stp
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
  toSymbol Arg = 'R'
  toSymbol Asn = 'N'
  toSymbol Asp = 'D'
  toSymbol Cys = 'C'
  toSymbol Gln = 'Q'
  toSymbol Glu = 'E'
  toSymbol Gly = 'G'
  toSymbol His = 'H'
  toSymbol Ile = 'I'
  toSymbol Leu = 'L'
  toSymbol Lys = 'K'
  toSymbol Met = 'M'
  toSymbol Phe = 'F'
  toSymbol Pro = 'P'
  toSymbol Pyl = 'O'
  toSymbol Sec = 'U'
  toSymbol Ser = 'S'
  toSymbol Thr = 'T'
  toSymbol Trp = 'W'
  toSymbol Tyr = 'Y'
  toSymbol Val = 'V'
  toSymbol Stp = '*'

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

translate :: T.Text -> [AminoAcid]
translate str = if T.length str `mod` 3 /= 0
                then trace "-- Invalid length" error "Invalid length"
                else map ( traceCodonToAmino . T.toUpper . traceShowId) $ trace ("-- " ++ show (T.chunksOf 3 str)) $ T.chunksOf 3 str
  where
    traceCodonToAmino :: T.Text -> AminoAcid
    traceCodonToAmino codon = case M.lookup codon codonTable of
                           Just amino -> amino
                           Nothing    -> trace ("Invalid codon: " ++ T.unpack codon) error "Invalid codon"
    -- It took more time to write this table than to write the else of the hw btw
    codonTable :: M.Map T.Text AminoAcid
    codonTable = M.fromList [("GCT", Ala), ("GCC", Ala), ("GCA", Ala), ("GCG", Ala),
                             ("CGT", Arg), ("CGC", Arg), ("CGA", Arg), ("CGG", Arg), ("AGA", Arg), ("AGG", Arg),
                             ("AAT", Asn), ("AAC", Asn),
                             ("GAT", Asp), ("GAC", Asp),
                             ("TGT", Cys), ("TGC", Cys),
                             ("CAA", Gln), ("CAG", Gln),
                             ("GAA", Glu), ("GAG", Glu),
                             ("GGT", Gly), ("GGC", Gly), ("GGA", Gly), ("GGG", Gly),
                             ("CAT", His), ("CAC", His),
                             ("ATT", Ile), ("ATC", Ile), ("ATA", Ile),
                             ("TTA", Leu), ("TTG", Leu), ("CTT", Leu), ("CTC", Leu), ("CTA", Leu), ("CTG", Leu),
                             ("AAA", Lys), ("AAG", Lys),
                             ("ATG", Met),
                             ("TTT", Phe), ("TTC", Phe),
                             ("CCT", Pro), ("CCC", Pro), ("CCA", Pro), ("CCG", Pro),
                             ("TAA", Stp), ("TAG", Stp), ("TGA", Stp),
                             ("ACT", Thr), ("ACC", Thr), ("ACA", Thr), ("ACG", Thr),
                             ("TGG", Trp),
                             ("TAT", Tyr), ("TAC", Tyr),
                             ("GTT", Val), ("GTC", Val), ("GTA", Val), ("GTG", Val),
                             ("TAA", Stp), ("TGA", Stp), ("TAG", Stp)]

