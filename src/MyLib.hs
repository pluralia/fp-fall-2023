{- cabal:
build-depends: base, containers, text, vector
-}

{-# LANGUAGE OverloadedLists   #-}

-- hlint предложил убрать прагму OverloadedStrings, но я погуглила и вроде эта прагма позволяет нам 
-- работать со значениями, отличными от string и в общем случае при работе с Data.Text она нужна. Не убираю
{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import           Data.Char       (ord)
-- здесь при сборке cabal пишет, что импорт foldl', foldr из Data.Foldable избыточен, но я тогда не поняла, откуда мы его берем
import           Data.Foldable   (toList, foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Data.Set        as S -- для 10 задания

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
padZero str width 
  | T.length str >= width = str
  | otherwise = T.replicate (width - T.length str) "0" <> str

------------------------------------------------------------------------------------------------

-- 2. Шифр (0,5 балла)
--
-- Может пригодиться документация пакета vector: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html

cypher :: V.Vector Char
cypher = ['X', 'W', 'P', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
          'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'Q', 'R',
          'S', 'T', 'U', 'V', 'Y', 'Z'] -- произвольно дополнить до 26 символов

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
evenodd = foldr (\x (odd1, even1) -> (x:even1, odd1)) ([], [])

-- тут явно не сказано, какой список должен выводиться первым - с четными или с нечетными позициями
-- я вывожу нечетные-четные, если надо наоборот, то
{-
evenodd :: [a] -> ([a], [a])
evenodd = foldr (\x (even2, odd2) -> (x:odd2, even2)) ([], [])
-}

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average vec | V.length vec > 0 = V.foldl' (+) 0 vec / fromIntegral (V.length vec)
            | otherwise = error "Nothing to count - empty list"

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

nuclCheck :: Char -> Char -> Int
nuclCheck n1 n2 | n1 == n2 = 1
              | otherwise = 0

gcContent :: T.Text -> Double
gcContent str | T.length str > 0 = fromIntegral gcTotal / fromIntegral (T.length str)
              | otherwise = error "Nothing to count - empty string"
    where
      gcIsland x = nuclCheck 'G' x + nuclCheck 'C' x
      gcTotal = T.foldl'(\c x -> c + gcIsland x) 0 str
      

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
isReversePalindrom str = str == T.reverse (T.map (reverseNucl M.!) str)
    where
      reverseNucl :: M.Map Char Char
      reverseNucl = M.fromList [('A', 'T'), ('T', 'A'), ('C', 'G'), ('G', 'C')]


------------------------------------------------------------------------------------------------

-- 7. Температура плавления (0,75 балла)
--
-- Температура плавления короткой последовательности ДНК — важная характеристика для лабораторных экспериментов.
-- Её можно приближённо посчитать по формуле "4 * (число букв G или C) + 2 * (число букв A или T)" градусов.
--
-- Посчитать используя свёртку за один проход по последовательности.

-- использовала nuclCheck из 5 задания

meltingTemp :: T.Text -> Int
meltingTemp = T.foldl' (\c x -> c + nMeltingTemp x) 0
    where
        gcIsland x = nuclCheck 'G' x + nuclCheck 'C' x
        atIsland x = nuclCheck 'A' x + nuclCheck 'T' x
        nMeltingTemp nucl = 4 * gcIsland nucl + 2 * atIsland nucl

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

hamming :: T.Text -> T.Text -> Int
hamming str1 str2 = sum $ map (\(c1, c2) -> if c1 /= c2 then 1 else 0) $ T.zip str1 str2

identity :: T.Text -> T.Text -> Double
identity str1 str2 | T.length str1 == T.length str2 && T.length str1 > 0 = 1 - fromIntegral (hamming str1 str2) / fromIntegral (T.length str1)
                   | otherwise = error "Different length"

------------------------------------------------------------------------------------------------

-- 9. M.fromList (1 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

--разница в том, какое значение при перезаписи (если значения ключа повторяются) будет оставаться - 
--правая или левая (т.к. правая и левая свертки смотрят с разных концов)
fromListL :: Ord k => [(k, v)] -> M.Map k v
fromListL = foldl' (flip(uncurry M.insert)) M.empty

fromListR :: Ord k => [(k, v)] -> M.Map k v
fromListR = foldr (uncurry M.insert) M.empty

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
nubOrd = toList . foldr S.insert S.empty

------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery = undefined

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

data AminoAcid = Ala | Cys | Asp | Glu | Phe | Gly | His | Ile | Lys | Leu 
               | Met | Asn | Pro | Gln | Arg | Ser | Thr | Val | Trp | Tyr | Term
               deriving (Eq, Show, Ord)

{- 
-- тут у меня скорее биологический вопрос по кодонам Pyl и Sec - в основной тип я их не записываю, 
-- потому что у них странные кодоны, которые совпадают со стоп кодоном, хотя и однобуквенный символ у них тоже есть
-}
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
    toSymbol Pro = 'P'
    toSymbol Gln = 'Q'
    toSymbol Arg = 'R'
    toSymbol Ser = 'S'
    toSymbol Thr = 'T'
    toSymbol Val = 'V'
    toSymbol Trp = 'W'
    toSymbol Tyr = 'Y'
    toSymbol Term = '*'
    --toSymbol Pyl = 'O'
    --toSymbol Sec = 'U'
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

codonList :: [(AminoAcid, [String])]
codonList = [
  (Ala, ["GCU", "GCC", "GCA", "GCG"])
  , (Cys, ["TGT", "TGC"])
  , (Asp, ["GAT", "GAC"])
  , (Glu, ["GAA", "GAG"])
  , (Phe, ["TTT", "TTC"])
  , (Gly, ["GGT", "GGC", "GGA", "GGG"])
  , (His, ["CAT", "CAC"])
  , (Ile, ["ATT", "ATC", "ATA"])
  , (Lys, ["AAA", "AAG"])
  , (Leu, ["TTA", "TTG", "CTT", "CTC", "CTA", "CTG"])
  , (Met, ["ATG"])
  , (Asn, ["AAT", "AAC"])
  , (Pro, ["CCT", "CCC", "CCA", "CCG"])
  , (Gln, ["CAA", "CAG"])
  , (Arg, ["CGT", "CGC", "CGA", "CGG", "AGA", "AGG"])
  , (Ser, ["TCT", "TCC", "TCA", "TCG", "AGT", "AGC"])
  , (Thr, ["ACT", "ACC", "ACA", "ACG"])
  , (Val, ["GTT", "GTC", "GTA", "GTG"])
  , (Trp, ["TGG"])
  , (Tyr, ["TAT", "TAC"])
  , (Term, ["TAA", "TAG", "TGA"])]

translate :: T.Text -> [AminoAcid]
translate str | T.length str `mod` 3 /= 0 = error "Incorrect lenght of DNA"
              | otherwise                 = map (codonTable M.!) . T.chunksOf 3 $ str where
                codonTable = M.fromList $ concatMap (\(aName, aCodon) -> map (\c -> (T.pack c, aName)) aCodon) codonList


