{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib where

{- cabal:
build-depends: base, containers, text, vector
-}


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
padZero str width
  | T.length str >= width = str
  | otherwise = T.replicate (width - T.length str) "0" <> str

------------------------------------------------------------------------------------------------

-- 2. Шифр (0,5 балла)
--
-- Может пригодиться документация пакета vector: https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html

cypher :: V.Vector Char
cypher = [ 'X', 'W', 'P', 'A', 'T', 'N', 'M', 'O', 'S', 'V',
           'R', 'Q', 'I', 'U', 'C', 'B', 'D', 'F', 'H', 'J',
           'K', 'L', 'Y', 'Z', 'G', 'E'] -- произвольно дополнить до 26 символов

-- >>> index 'A'
-- 0
-- >>> index 'H'
-- 7
index :: Char -> Int
index c = ord c - ord 'A'

-- Считаем что приходят только заглавные английские буквы
encode :: T.Text -> T.Text
encode str = T.pack $ map (\c -> cypher V.! index c) (T.unpack str)

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

evenodd :: [a] -> ([a], [a])
evenodd = foldr (\x (ev, od) -> (x:od, ev)) ([], [])

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average vec = V.foldl' (+) 0 vec / fromIntegral (V.length vec)

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

gcContent :: T.Text -> Double
gcContent = (/) <$> fromIntegral . T.length . T.filter (\c -> c == 'G' || c == 'C') <*> fromIntegral . T.length


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
isReversePalindrom str = str == T.reverse (T.map complement str)
  where
    complement 'A' = 'T'
    complement 'T' = 'A'
    complement 'G' = 'C'
    complement 'C' = 'G'
    complement _   = error "Invalid character in DNA sequence"
------------------------------------------------------------------------------------------------

-- 7. Температура плавления (0,75 балла)
--
-- Температура плавления короткой последовательности ДНК — важная характеристика для лабораторных экспериментов.
-- Её можно приближённо посчитать по формуле "4 * (число букв G или C) + 2 * (число букв A или T)" градусов.
--
-- Посчитать используя свёртку за один проход по последовательности.

meltingTemp :: T.Text -> Int
meltingTemp = T.foldl' (\temp c -> temp + case c of
                                            'G' -> 4
                                            'C' -> 4
                                            'A' -> 2
                                            'T' -> 2
                                            _   -> error "Invalid character in DNA sequence"
                        ) 0

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
identity sequence1 sequence2
    | validInputLength = calculateIdentity sequence1 sequence2
    | otherwise        = error "Invalid identity call"
    where
        validInputLength = lengthSequence1 == lengthSequence2 && lengthSequence1 > 0
        lengthSequence1 = T.length sequence1
        lengthSequence2 = T.length sequence2

calculateIdentity :: T.Text -> T.Text -> Double
calculateIdentity s1 s2 = fromIntegral (matchingCount s1 s2) / fromIntegral (T.length s1)

matchingCount :: T.Text -> T.Text -> Int
matchingCount s1 s2 = sum $ map (\(c1, c2) -> if c1 == c2 then 1 else 0) $ T.zip s1 s2




-----------------------------------------------------------------------------------------------

-- 9. M.fromList (1 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: Ord k => [(k, v)] -> M.Map k v
fromListL = foldl' (\m (k, v) -> M.insert k v m) M.empty
-- Она начинает с пустой карты M.empty и на каждом шаге добавляет
-- очередную пару ключ-значение с помощью M.insert.


fromListR :: Ord k => [(k, v)] -> M.Map k v
fromListR = foldr (\(k, v) m -> M.insert k v m) M.empty
-- Эта реализация использует foldr для обхода списка в обратном порядке.
-- Она также начинает с пустой карты M.empty и добавляет очередную пару ключ-значение
-- Поведение различается в том, как элементы добавляются в карту и в каком порядке они будут в ней находиться
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
nubOrd = S.toList . foldr S.insert S.empty
------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery = T.intercalate "&" . M.foldrWithKey (\k v acc -> k `T.append` "=" `T.append` v : acc) []

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

data AminoAcid = Ala   -- Аланин
              | Arg   -- Аргинин
              | Asn   -- Аспарагин
              | Asp   -- Аспарагиновая кислота
              | Cys   -- Цистеин
              | Gln   -- Глутамин
              | Glu   -- Глутаминовая кислота
              | Gly   -- Глицин
              | His   -- Гистидин
              | Ile   -- Изолейцин
              | Leu   -- Лейцин
              | Lys   -- Лизин
              | Met   -- Метионин
              | Phe   -- Фенилаланин
              | Pro   -- Пролин
              | Ser   -- Серин
              | Thr   -- Треонин
              | Trp   -- Триптофан
              | Tyr   -- Тирозин
              | Val   -- Валин
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
  toSymbol Ala   = 'A'
  toSymbol Arg   = 'R'
  toSymbol Asn   = 'N'
  toSymbol Asp   = 'D'
  toSymbol Cys   = 'C'
  toSymbol Gln   = 'Q'
  toSymbol Glu   = 'E'
  toSymbol Gly   = 'G'
  toSymbol His   = 'H'
  toSymbol Ile   = 'I'
  toSymbol Leu   = 'L'
  toSymbol Lys   = 'K'
  toSymbol Met   = 'M'
  toSymbol Phe   = 'F'
  toSymbol Pro   = 'P'
  toSymbol Ser   = 'S'
  toSymbol Thr   = 'T'
  toSymbol Trp   = 'W'
  toSymbol Tyr   = 'Y'
  toSymbol Val   = 'V'
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

translate :: T.Text -> [AminoAcid]
translate str
  | T.length str `mod` 3 /= 0 = error "Input length is not a multiple of 3"
  | otherwise = T.chunksOf 3 str >>= (\codon -> case M.lookup codon codonTable of
                                                Just amino -> [amino]
                                                Nothing    -> [Stop])
  where
    codonTable = M.fromList
      [ ("TTT", Phe), ("TTC", Phe), ("TTA", Leu), ("TTG", Leu)
      , ("TCT", Ser), ("TCC", Ser), ("TCA", Ser), ("TCG", Ser)
      , ("TAT", Tyr), ("TAC", Tyr), ("TAA", Stop), ("TAG", Stop)
      , ("TGT", Cys), ("TGC", Cys), ("TGA", Stop), ("TGG", Trp)
      , ("CTT", Leu), ("CTC", Leu), ("CTA", Leu), ("CTG", Leu)
      , ("CCT", Pro), ("CCC", Pro), ("CCA", Pro), ("CCG", Pro)
      , ("CAT", His), ("CAC", His), ("CAA", Gln), ("CAG", Gln)
      , ("CGT", Arg), ("CGC", Arg), ("CGA", Arg), ("CGG", Arg)
      , ("ATT", Ile), ("ATC", Ile), ("ATA", Ile), ("ATG", Met)
      , ("ACT", Thr), ("ACC", Thr), ("ACA", Thr), ("ACG", Thr)
      , ("AAT", Asn), ("AAC", Asn), ("AAA", Lys), ("AAG", Lys)
      , ("AGT", Ser), ("AGC", Ser), ("AGA", Arg), ("AGG", Arg)
      , ("GTT", Val), ("GTC", Val), ("GTA", Val), ("GTG", Val)
      , ("GCT", Ala), ("GCC", Ala), ("GCA", Ala), ("GCG", Ala)
      , ("GAT", Asp), ("GAC", Asp), ("GAA", Glu), ("GAG", Glu)
      , ("GGT", Gly), ("GGC", Gly), ("GGA", Gly), ("GGG", Gly)
      ]