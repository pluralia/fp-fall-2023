module Parser where

import Control.Applicative (Alternative (..))
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.Foldable (foldl')

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g aP = Parser f
    where
      f s = case runParser aP s of
        Nothing -> Nothing
        Just (a, s') -> Just (g a, s')

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fP <*> aP = Parser f
    where
      f s = case runParser fP s of
        Nothing -> Nothing
        Just (g, s') -> case runParser aP s' of
          Nothing -> Nothing
          Just (a, s'') -> Just (g a, s'')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  pA <|> pA' = Parser f
    where
      f s = case runParser pA s of
        Nothing -> runParser pA' s
        x -> x

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f [] = Nothing
    f (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

symbolP :: Parser Char
symbolP = satisfyP isAlphaNum

symbolsP :: Parser String
symbolsP = some symbolP

oneSpaceP :: Parser Char
oneSpaceP = satisfyP isSpace

spaceP :: Parser String
spaceP = many oneSpaceP

digitP :: Parser Int
digitP = digitToInt <$> satisfyP isDigit

digitsP :: Parser [Int]
digitsP = some digitP

-- | Как использовать парсер
parseNumber :: Maybe ([Int], String)
parseNumber = runParser digitsP "123 23 AB" -- Just ([1,2,3], " 23 AB")

-- Atom <$> nameM <*> coordsM
-- parser -> function that turns string into object

-- runParser (pure (+) <*> digitP <*> digitP) -> «12AB» = Just (3, «AB»)