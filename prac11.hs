import System.IO         (IOMode (..), hPutStrLn, withFile, hGetContents)
import Control.DeepSeq   (NFData, force)
import Control.Monad     (forM, forM_, join)
import Control.Exception (evaluate)

------------------------------------------------------------------------------

-- 1. Определите, как каждая из этих функций будет вычислять свои аргументы
--     не вычисляет (нестрогая по аргументу)
--     вычисляет до (строгая по аргументу)
--        1) NF
--        2) WHNF

concatenate :: [a] -> [a] -> [a]
concatenate []     l = l
concatenate (x:xs) l = x : concatenate xs l
-- 1 аргумент строго, WHNF
-- 2 аргумент нестрого

mix :: [a] -> [a] -> [a]
mix []       l2 = l2
mix l1       [] = l1
mix (x1:x1s) (x2:x2s) = x1 : x2 : mix x1s x2s
-- 1 и 2 аргументы строго, WHNF

silly :: [Int] -> [a] -> [a]
silly [] _ = []
silly _ [] = []
silly (x:xs) (y:ys) | x == 0    = silly xs ys
                    | otherwise = y : silly xs ys
-- 1 аргумент строго, NF
-- 2 аргумент строго, WHNF

------------------------------------------------------------------------------

-- 2. Распишите вычисление callF в thunk'ах

f :: Int -> Int -> Int
f x y = if x > 0 then x - 1 else x + 1

f' :: Num a => a -> a
f' x = x - 1

callF :: Int
callF = f (f' 3) 2

-- 1 step
-- <thunk: f <thunk: (f' 3)> <thunk: 2>>

-- 2 step
-- <thunk: if <thunk: <thunk: (f' 3)> > 0> then <thunk: (f' 3)> - 1 else <thunk: (f' 3)> + 1>>

-- 3 step
-- <thunk: if <thunk: <thunk: <thunk: 3> - 1> > 0> then <thunk: (f' 3)> - 1 else <thunk: (f' 3)> + 1>>

-- 4 step
-- <thunk: if <thunk: <thunk: 3 - 1> > 0> then <thunk: (f' 3)> - 1 else <thunk: (f' 3)> + 1>>

-- 5 step
-- <thunk: if <thunk: 2 > 0> then <thunk: (f' 3)> - 1 else <thunk: (f' 3)> + 1>>

-- 6 step
-- <thunk: if True then <thunk: (f' 3)> - 1 else <thunk: (f' 3)> + 1>>

-- 7 step
-- <thunk: <thunk: (f' 3)> - 1>

-- 8 step
-- <thunk: 2 - 1>

-- 9 step
-- <thunk: 1>

-- 10 step
-- 1

------------------------------------------------------------------------------

-- Ленивое IO

filePath :: FilePath
filePath = "example.txt"

printExample :: IO ()
printExample = do
    fileS <- readFile filePath
    putStrLn fileS

-- withFile -- открытия хэндла -> переданное действие над хэндлом -> закрытие хэндла
-- Опасность в том, что закрытие хэндла происходит __энергично__ (то есть после вызова withFile хэндл
-- гарантированно будет закрыт). Поэтому если действие над хэндлом выполняется лениво,
-- вычисление результата происходит сильно после того, как хэндл закрыли.

lazyExample :: IO ()
lazyExample = do
    c <- withFile filePath ReadMode $ \h -> do
        hGetContents h
    putStrLn "passed"
    print c

-- | Чтобы это побороть, можно сделать действие над хэндлом энергичным, обернув его в функцию forceEvalIO.
--
forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

eagerExample :: IO ()
eagerExample = do
    c <- withFile filePath ReadMode $ \h -> forceEvalIO $ do
        hGetContents h
    putStrLn "passed"
    print c

-- Важно! Оборачивание всего withFile в forceEvalIO не решает проблему:
-- в forceEvalIO передаётся пусть и ленивый, но __результат вызова__ withFile.
-- Если вызов withFile произошёл, то хэндл __уже закрыт__, а считыание файла ещё не началось,
-- потому что отложено из-за ленивости.

------------------------------------------------------------------------------

-- Разбираемся в связи do-нотации и >>=

fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
fromDo11 aM bM = do
    a <- fmap (+ 10) aM
    let aL = [a, a, a]
        a  = a + length aL
    return a
    bM
    [a, b, c] <- Just aL
    b <- fmap (<> "abcd") bM
    pure (c, b)

-- (>>=) :: m a -> (a -> m b) -> m b
--   * monad >>= (\x -> eval x)
-- или
--   * monad >>= (\x ->
--     eval x)

-- конвертируется в

-- do
--     x <- monad
--     eval x

----------------------------

-- (>>) :: m a -> m b -> m b
--   * monad1 >> monad2

-- конвертируется в

-- do
--   monad1
--   monad2

fromDo11 aM bM = fmap (+ 10) aM >>= \a ->
    let aL = [a, a, a]
        a  = a + length aL
    in return a >> bM >> do
        [a, b, c] <- Just aL
        b <- fmap (<> "abcd") bM
        pure (c, b)

-- Зачем нам нужен return?
-- Чтобы можно было в общем виде оборачивать в любой тип, для которого реализован
-- класс типов Monad
funcMaybe :: Monad m => m Int
funcMaybe = return 23 >>= \x -> return $ succ x

------------------------------------------------------------------------------
