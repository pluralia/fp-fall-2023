{-# LANGUAGE InstanceSigs #-}

module Parser where

import Data.Char           (digitToInt, isAlphaNum, isSpace, isDigit)
import Control.Applicative (Alternative (..))
import Data.Foldable       (foldl')
import Data.Functor        (($>))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap g aP = Parser f
      where
        f s = case runParser aP s of
            Nothing      -> Nothing
            Just (a, s') -> Just (g a, s')

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fP <*> aP = Parser f
    where
      f s = case runParser fP s of
        Nothing      -> Nothing
        Just (g, s') -> case runParser aP s' of
          Nothing       -> Nothing
          Just (a, s'') -> Just (g a, s'')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  pA <|> pA' = Parser f
    where
      f s = case runParser pA s of
        Nothing -> runParser pA' s
        x       -> x

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f []       = Nothing
    f (x : xs) | p x       = Just (x, xs)
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

intP :: Parser Int
intP = foldl' (\acc x -> acc * 10 + x) 0 <$> some digitP

signP :: Num a => Parser (a -> a)
signP = (satisfyP (== '-') $> negate) <|> pure id

floatP :: Char -> Parser Float
floatP sign = (\ f a -> f . (+) (fromIntegral a))
  <$> signP
  <*> intP
  <* satisfyP (== sign)
  <*> helper
  where
    helper :: Parser Float
    helper = foldl' (\ acc x -> 0.1 * (acc + fromIntegral x)) 0.0 . reverse <$> digitsP

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:)
  <$> element
  <*> many (sep *> element)
  <|> pure []

newLineP :: Parser Char
newLineP = satisfyP (== '\n')

-- | Как использовать парсер
--
parseNumber :: Maybe ([Int], String)
parseNumber = runParser digitsP "123 23 AB" -- Just ([1,2,3], " 23 AB")
