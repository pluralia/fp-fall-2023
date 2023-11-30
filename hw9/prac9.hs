import qualified Data.Map.Strict as M
import Control.Monad
import Data.Ix
import Data.List
import Data.Monoid

-- cabal install --lib  mtl
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Control.Monad.State

-------------------------------------------------------------------------------

-- Maybe

safeSqrt :: (Floating a, Ord a) => a -> Maybe a
safeSqrt x
    | x /= 0    = Just (sqrt x)
    | otherwise = Nothing

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
    | x > 0     = Just (log x)
    | otherwise = Nothing       -- log на отрицательном числе вернет NaN

logAndSqrt :: (Floating a, Ord a) => a -> Maybe a
logAndSqrt num = safeSqrt num >>= safeLog

logAndSqrtFish :: (Floating a, Ord a) => a -> Maybe a
logAndSqrtFish = safeSqrt >=> safeLog

-------------------------------------------------------------------------------

-- Either

safeSqrtE :: Float -> Either String Float
safeSqrtE x
    | x /= 0    = Right (sqrt x)
    | otherwise = Left "Division by 0 is forbidden!"

safeLogE :: Float -> Either String Float
safeLogE x
    | x > 0     = Right (log x)
    | otherwise = Left "Log can't work properly on negative numbers!"

logAndSqrtFishE :: Float -> Either String Float
logAndSqrtFishE = safeSqrtE >=> safeLogE

safeEvenE :: Float -> Either String Float
safeEvenE x
    | even $ floor x = Right x
    | otherwise      = Left "The result is not even!"

-- logAndSqrtAndFilterE :: Float -> Either String Float
-- logAndSqrtAndFilterE x = do
--     sqrtRes <- safeSqrtE x
--     logRes <- safeLogE sqrtRes
--     safeEvenE logRes

logAndSqrtAndFilterE :: Float -> Either String Float
logAndSqrtAndFilterE x =
    safeSqrtE x >>= (\sqrtRes ->
        safeLogE sqrtRes >>= (\logRes ->
            safeEvenE logRes
        )
    )

-------------------------------------------------------------------------------

-- List

type Pos = (Int, Int)

-- говорит, если доска 8x8 полностью покрыта ходами
isCovered :: [Pos] -> Bool
isCovered = undefined

makeStep :: Pos -> [Pos]
makeStep (x, y) =
  [(newX, newY) | newX <- [x - 1 .. x + 1], newY <- [y - 1 .. y + 1], (1, 8) `inRange` newX && (1, 8) `inRange` newY]

-- threeTurns :: Pos -> [Pos]
-- threeTurns pos = do
--   posStep1 <- makeStep pos
--   posStep2 <- makeStep posStep1
--   posStep3 <- makeStep posStep2
--   return posStep3

threeTurns :: Pos -> [Pos]
threeTurns pos =
    makeStep pos >>= (\posStep1 ->
        makeStep posStep1 >>= (\posStep2 ->
            makeStep posStep2 >>= (\posStep3 ->
                return posStep3
            )
        )
    )

threeTurns' :: Pos -> [Pos]
threeTurns' pos = [posStep3 | posStep1 <- makeStep pos, posStep2 <- makeStep posStep1, posStep3 <- makeStep posStep2]

-- (>>=) :: [a] -> (a -> [b]) -> [b]
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- >>= ~ concatMap
threeTurns'' :: Pos -> [Pos]
threeTurns'' pos = concatMap makeStep . concatMap makeStep . concatMap makeStep $ [pos]

-------------------------------------------------------------------------------

-- Writer

-- Без Writer
sqrL :: Int -> (Int, String)
sqrL x = (x * x, "Square of " <> show x <> " | ")

sqrLThreeTimes :: Int -> (Int, String)
sqrLThreeTimes x =
    let (res1, log1) = sqrL x
        (res2, log2) = sqrL res1
        (res3, log3) = sqrL res2
     in (res3, mconcat [log1, log2, log3])

-- C Writer
-- воспользуемся библиотечным
-- https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-Lazy.html#g:2

sqrLW :: Int -> Writer String Int
sqrLW = writer . sqrL

-- sqrLThreeTimesW :: Int -> Writer String Int
-- sqrLThreeTimesW x = do
--     res1 <- sqrLW x
--     res2 <- sqrLW res1
--     sqrLW res2

sqrLThreeTimesW :: Int -> Writer String Int
sqrLThreeTimesW x =
    sqrLW x >>= (\res1 ->
        sqrLW res1 >>= (\res2 ->
            sqrLW res2
        )
    )

---------------------------------------

stocks :: [(String, Int)]
stocks = [("Identity", 0), ("Maybe", 5),
          ("List", 8), ("Writer", 10),
          ("Reader", 10), ("State", 10),
          ("IO", 50), ("STM", 100)
         ]

-- addMonad :: String -> Int -> Writer (Sum Int) String
-- addMonad monad qty = do
--     let Just price = lookup monad stocks
--     writer (monad, Sum $ price * qty)

addMonad :: String -> Int -> Writer (Sum Int) String
addMonad monad qty =
    let Just price = lookup monad stocks
     in writer (monad, Sum $ price * qty)

discount :: Int -> Sum Int -> Sum Int
discount proc (Sum x) =
  if x < 100 then Sum x
  else Sum $ x * (100 - proc) `div` 100

cart :: Writer (Sum Int) [String]
cart = do
    x1 <- addMonad "Identity" 1
    x2 <- addMonad "Maybe" 1
    x3 <- addMonad "List" 1
    -- x4 <- addMonad "STM" 1
    -- censor :: (w -> w) -> Writer w a -> Writer w a
    x4 <- censor (discount 25) (addMonad "STM" 1)
    -- x5 <- addMonad "IO" 1
    -- listens :: (w -> b) -> Writer w a -> Writer w (a, b)
    (x5, fromLog) <- listens (Sum . (* 2) . getSum) (addMonad "IO" 1)
    -- (addMonad "IO" 1) ~ ("IO", Sum 50)
    -- tell fromLog
    pure [x1, x2, x3, x4, x5]

-- listens f (Writer (x, log)) = Writer ((x, f log), log)
--  censor f (Writer (x, log)) = Writer (x,          f log)

cartNotDo :: Writer (Sum Int) [String]
cartNotDo =
    addMonad "Identity" 1 >>= (\x1 ->
        addMonad "Maybe" 1 >>= (\x2 ->
            addMonad "List" 1 >>= (\x3 ->
                censor (discount 25) (addMonad "STM" 1) >>= (\x4 ->
                    listens (Sum . (* 2) . getSum) (addMonad "IO" 1) >>= (\(x5, fromLog) ->
                        pure [x1, x2, x3, x4, x5]
                    )
                )
            )
        )
    )

-------------------------------------------------------------------------------

-- Reader

-- | Вам приходит на вход выражение в виде бинарного дерева 
--
data Expr
  = Primary { item :: Item }  -- значение в листьях
  | Binary                    -- операция в узлах, но договоримся, что пока у нас возможно только сложение
    { left  :: Expr
    , right :: Expr
    }
  deriving (Show, Eq)

-- | Элементы могут быть переменными (тогда их нужно найти в окружении) или числами
--
data Item = Var String | Val Int
  deriving (Show, Eq)

-- | Окружение задается Map
--
type Environment = M.Map String Int

-- | Вычислите выражение, используя Reader
--
eval :: Expr -> Reader Environment (Maybe Int)
eval (Primary (Val x)) = return . Just $ x
eval (Primary (Var x)) = reader $ \r -> M.lookup x r
-- eval (Binary left right) = do
--     resLeft <- eval left
--     resRight <- eval right
--     return $ do
--         x <- resLeft
--         y <- resRight
--         return $ x + y
eval (Binary left right) =
    eval left >>= (\resLeft ->
        eval right >>= (\resRight ->
            pure $ resLeft >>= (\x -> resRight >>= (\y -> pure $ x + y))
        )
    )

-- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
-- (>>=) :: Reader Environment (Maybe Int) -> ((Maybe Int) -> Reader Environment (Maybe Int)) -> Reader Environment (Maybe Int)
-- f  >>= k = Reader $ \e -> runReader (k (runReader f e)) e
-- eval left :: Reader Environment (Maybe Int)
-- eval left >>= (\resLeft -> ...)
-- runReader f e ~ runReader (eval left) e :: a ~ Maybe Int
-- k ~ (\resLeft -> ...) :: (Maybe Int) -> Reader Environment (Maybe Int)
-- k (runReader f e) ~ (\resLeft -> ...) runReader (eval left) e :: Reader Environment (Maybe Int)
-- Reader $ \e -> runReader ((\resLeft -> ...) runReader (eval left) e) e

data Stmt = Stmt
    { name :: String
    , expr :: Expr
    } deriving (Show, Eq)

-- | Вычислите утверждения, используя Reader
--
evalStmts :: [Stmt] -> Reader Environment (Maybe Int)
evalStmts []                      = return Nothing
evalStmts [Stmt _ expr]           = eval expr
-- evalStmts ((Stmt name expr) : xs) = do
--     maybeVal <- eval expr
--     env <- ask
--     local (maybe id (M.insert name) maybeVal) (evalStmts xs)
evalStmts ((Stmt name expr) : xs) =
    eval expr >>= (\maybeVal -> 
        ask >>= (\env ->
            local (maybe id (M.insert name) maybeVal) (evalStmts xs)
        )
    )


testEval :: Maybe Int
testEval = runReader (eval expr) env
  where
    env :: Environment
    env = M.fromList [("x", 3)]

    expr :: Expr
    expr = Binary (Primary . Val $ 2) (Primary . Var $ "x")

testEvalStmts :: Maybe Int
testEvalStmts = runReader (evalStmts [x, y, z, xx, w]) M.empty
  where
    x, y, z, xx, w :: Stmt
    x = Stmt "x" $ Primary . Val $ 2                                     -- x = 2
    y = Stmt "y" $ Primary . Val $ 3                                     -- y = 3
    z = Stmt "z" $ Binary (Primary . Var $ "x") (Primary . Var $ "y")    -- z = 5
    xx = Stmt "x" $ Binary (Primary . Var $ "x") (Primary . Var $ "x")   -- xx = 4
    w = Stmt "w" $ Binary (Primary . Var $ "z") (Primary . Var $ "x")    -- w = 9

-------------------------------------------------------------------------------

-- State

data SpeakerPower = Sleep | Work
  deriving (Show, Eq)
type Level        = Int
type Speaker      = (SpeakerPower, Level)

increase :: Level -> Level
increase i = min 11 (i + 1)

decrease :: Level -> Level
decrease i = max 0 (i - 1)

data Action = Button | VolumeUp | VolumeDown
  deriving (Show, Eq)

trans :: Action -> Speaker -> Speaker
trans Button (Sleep, n) = (Work, n)
trans Button  (Work, n) = (Sleep, n)
trans VolumeUp   (s, n) = (s, increase n)
trans VolumeDown (s, n) = (s, decrease n)

actions :: [Action]
actions = 
  [ Button, VolumeUp
  , VolumeUp, VolumeDown
  , Button
  ]

play :: Speaker -> Speaker
play initialState = foldr trans initialState actions

type FSM s = State s s   -- finite-state machine

-- fsm :: (Action -> Speaker -> Speaker) -> Action -> FSM Speaker
fsm :: (a -> s -> s) -> a -> FSM s
fsm trans act = state $ \s -> (s, trans act s)

playM :: State Speaker [Speaker]
-- mapM :: (a -> m b) -> t a -> m (t b)
-- mapM :: (Action -> State Speaker Speaker) -> [Action] -> State Speaker [Speaker]
playM = mapM (fsm trans) actions

allInfo :: ([Speaker], Speaker)
allInfo = runState playM (Sleep, 0)

currentState :: Speaker
currentState = execState playM (Sleep, 0)

-- не включая последнее
passedStates :: [Speaker]
passedStates = evalState playM (Sleep, 0)