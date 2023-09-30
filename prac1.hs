data Nat = Zero | Suc Nat
  deriving (Show)


-- data [a] = [] | a : [a]

data List a = Nil | Cons a (List a)
  deriving (Show)


data MyBool = MyTrue | MyFalse
  deriving (Show)

data MyStudents = Kolya  | Yaroslav | Andrew
  deriving (Show)

data Student = Student String String Float
  deriving (Show)


-- data Maybe a = Nothing | Just a;

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe defaultCase f Nothing  = defaultCase
myMaybe _           f (Just x) = f x

-- myMaybe :: b -> (a -> b) -> Maybe a -> b

-- myMaybe 2 show Nothing -- error
-- 2 :: Int, b ==> b == Int
-- show :: Int -> String, (a -> b) ==> a == Int, b == String
-- b == Int and b = String -- conctradiction

-- myMaybe "123" show Nothing -- ok


-- data Either a b = Left a | Right b

myEither :: (b -> c) -> (a -> c) -> Either a b -> c
myEither f g (Left x)  = g x
myEither f g (Right x) = f x

-- Let's a :: Int, b :: Bool, c :: String
-- (b -> c) == (Bool -> String) --> can be (\x -> if x == True then "Apple" else "Orange")
-- (a -> c) == (Int -> String)  --> can be `show`
-- myEither (\x -> if x == True then "Apple" else "Orange") show (Left True)


myHead :: [a] -> Either String a
myHead []      = Left "The list is empty"
myHead (x : _) = Right x

myLast :: [a] -> Either String a
myLast []       = Left "The list is empty"
myLast [x]      = Right x
myLast (x : xs) = myLast xs


-- data (a, b) = (a, b)

birthday :: String -> (String, Int) -> (String, Maybe String, Int)
birthday patronymic (name, age) =
  if age >= 17
    then (name, Just patronymic, succ age)
    else (name, Nothing, succ age)

data Person = Person {
    name :: String,
    patronim :: Maybe String,
    age :: Int
}
  deriving (Show)

personBirthday :: String -> (String, Int) -> Person
personBirthday patronymic (name, age) =
  if age >= 17
    -- two ways to initialise an `Person`: 1. with records
    then Person { name,  patronim = Just patronymic, age = succ age }
    -- 2. without records
    else Person name Nothing (succ age)
