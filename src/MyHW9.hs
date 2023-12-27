module MyHW9 where
    {- cabal:
    build-depends: base, mtl, random
-}
import           Control.Monad.State.Lazy
import qualified System.Random            as R

------------------------------------------------------------------------------

-- 3. State: Генерация случайного значения кастомного типа (0,75 балла)

-- Чистый функциональный язык не может обновлять значения на месте
-- Распространенной идиомой для имитации таких вычислений с сохранением состояния является
-- "прохождение" параметра состояния через последовательность функций
-- Рассмотрим пример

data MyType = MT Int Bool Char Int
  deriving Show

-- Чтобы запустить: `makeRandomValue (mkStdGen 23)`

makeRandomValue :: R.StdGen -> (MyType, R.StdGen)
makeRandomValue g =
    let (n,g1) = R.randomR (1, 100) g
        (b,g2) = R.random g1
        (c,g3) = R.randomR ('a', 'z') g2
        (m,g4) = R.randomR (-n, n) g3
    in (MT n b c m, g4)

-- Этот подход работает, но такой код сложен в сопровождении, может содержать ошибки
-- и быть грязным (что делает его грязным?)
-- Монада State скрывает потоковую передачу состояния внутри операции >>=,
-- делая код проще для написания, чтения и модификации.

-- | Возвращает случайное значение и обновляет состояние генератора случайных чисел
--
getAny :: (R.Random a) => State R.StdGen a
getAny = do
    gen <- get
    let (val, gen1) = R.random gen
    put gen1
    return val

-- | Аналогична getAny, но генерирует значение в границах
--
getOne :: (R.Random a) => (a, a) -> State R.StdGen a
getOne bounds = do
    gen <- get
    let (val, gen1) = R.randomR bounds gen
    put gen1
    return val

-- | Используя монаду State с StdGen в качестве состояния, мы можем генерировать случаные значения
--   заданного типа, не передавая состояния генератора случайных чисел в коде вручную
--
makeRandomValueST :: R.StdGen -> (MyType, R.StdGen)
makeRandomValueST = runState (do
    n <- getOne (1, 100)
    b <- getAny
    c <- getOne ('a', 'z')
    m <- getOne (-n, n)
    return (MT n b c m))

------------------------------------------------------------------------------
