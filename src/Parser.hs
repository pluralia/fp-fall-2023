{-# LANGUAGE InstanceSigs #-}

module Parser where

import Data.Char           (digitToInt, isAlphaNum, isSpace, isDigit)
import Control.Applicative (Alternative (..))
import Data.Foldable       (foldl')

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

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) aP f = Parser bP
      where
        bP s = case runParser aP s of
          Nothing       -> Nothing
          Just (xx, s1) -> runParser (f xx) s1

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

stringP :: String -> Parser String
stringP str = Parser (helper str)
  where
    helper :: String -> String -> Maybe (String, String)
    helper []     x      = Just (str, x)
    helper _      []     = Nothing
    helper (s:ss) (x:xs) = if s == x then helper ss xs else Nothing

intP :: Parser Int
intP = foldl' (\acc x -> acc * 10 + x) 0 <$> some digitP

floatP :: Parser Float
floatP = (+) . fromIntegral
  <$> intP
  <* satisfyP (== '.')
  <*> helper
  where
    helper :: Parser Float
    helper = foldl' (\ acc x -> 0.1 * (acc + fromIntegral x)) 0.0 . reverse <$> digitsP

floatP' :: Parser Float
floatP' = ((*(-1.0 :: Float)) <$ satisfyP (=='-') <*> floatP) <|> floatP

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
