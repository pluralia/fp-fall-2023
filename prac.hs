module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n 
  | n > 0 = collatzHelper n 0
  | otherwise = Nothing
  where
    collatzHelper :: Integer -> Integer -> Maybe Integer
    collatzHelper 1 steps = Just steps
    collatzHelper n steps
      | even n = collatzHelper (n `div` 2) (steps + 1)
      | otherwise = collatzHelper (3 * n + 1) (steps + 1)