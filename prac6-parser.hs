import Data.Char           (digitToInt, isAlphaNum, isSpace, isDigit)
import Control.Applicative (Alternative (..))

newtype ParserS a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor ParserS where
    fmap :: (a -> b) -> ParserS a -> ParserS b
    fmap g aP = Parser f
      where
        f s = case runParser aP s of
            Nothing      -> Nothing
            Just (a, s') -> Just (g a, s')

instance Applicative ParserS where 
  pure :: a -> ParserS a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: ParserS (a -> b) -> ParserS a -> ParserS b
  fP <*> aP = Parser f
    where
      f s = case runParser fP s of
        Nothing      -> Nothing
        Just (g, s') -> case runParser aP s' of
          Nothing       -> Nothing
          Just (a, s'') -> Just (g a, s'')

instance Alternative ParserS where 
  empty :: ParserS a
  empty = Parser $ \_ -> Nothing

  (<|>) :: ParserS a -> ParserS a -> ParserS a
  pA <|> pA' = Parser f
    where
      f s = case runParser pA s of
        Nothing -> runParser pA' s
        x       -> x

satisfyP :: (Char -> Bool) -> ParserS Char
satisfyP p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f []       = Nothing
    f (x : xs) | p x       = Just (x, xs)
               | otherwise = Nothing

symbolP :: ParserS Char
symbolP = satisfyP isAlphaNum

oneSpaceP :: ParserS Char
oneSpaceP = satisfyP isSpace

digitP :: ParserS Int
digitP = digitToInt <$> satisfyP isDigit

digitsP :: ParserS [Int]
digitsP = some digitP

spaceP :: ParserS String
spaceP = many oneSpaceP

test :: Maybe ([Int], String)
test = runParser digitsP "123 23 AB"