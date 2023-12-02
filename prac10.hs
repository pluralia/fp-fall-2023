import           Control.Monad.State
import System.IO
import Control.Exception

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
actions =       -- initial: Sleep, 2
  [ Button      -- Work, 2
  , VolumeUp    -- Work, 3
  , VolumeUp    -- Work, 4
  , VolumeDown  -- Work, 3
  , Button      -- Sleep, 3
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
allInfo = runState playM (Sleep, 2)

currentState :: Speaker
currentState = execState playM (Sleep, 2)

-- не включая последнее
passedStates :: [Speaker]
passedStates = evalState playM (Sleep, 2)

-------------------------------------------------------------------------------

-- IO

-- Обработка ошибок

badHead :: [a] -> a
badHead []      = error "Empty list!"
badHead (x : _) = x

takeHead :: IO ()
takeHead = do
    fileLines <- lines <$> readFile "example.txt"
    
    forM_ fileLines $ \line ->
      print (badHead line) `catch` errHandler
  where
    errHandler :: SomeException -> IO ()
    errHandler _ = putStrLn "Found empty line"

-------------------------------------------------------------------------------