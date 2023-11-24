instance Traversable [] where
    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse _ []     = pure []
    traverse f (x:xs) = (:) <$> f x <*> traverse f xs
