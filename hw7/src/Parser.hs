{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}


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

--- MONAD FOR PARSER
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) aP f = Parser $ \s -> case runParser aP s of
    Nothing -> Nothing
    Just (result, rest) -> runParser (f result) rest

  return :: a -> Parser a
  return = pure

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f []       = Nothing
    f (x : xs) | p x       = Just (x, xs)
               | otherwise = Nothing

symbolP :: Parser Char
symbolP = satisfyP isAlphaNum

stringP :: String -> Parser String
stringP = traverse (satisfyP . (==))

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
intP = satisfyP (== '-') *> (negate <$> helper) <|> helper
  where
    helper :: Parser Int
    helper = foldl' (\ acc x -> 10 * acc + x) 0 <$> digitsP

floatP :: Parser Float
floatP = satisfyP (== '-') *> (negate <$> helper) <|> helper
  where
    helper :: Parser Float
    helper = do
      intPart <- fromIntegral . foldl' (\ acc x -> 10 * acc + x) 0 <$> digitsP
      _ <- satisfyP (== '.')
      floatPart <- foldr (\ x acc -> (acc + fromIntegral x) / 10) 0 <$> digitsP
      return $ intPart + floatPart

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
