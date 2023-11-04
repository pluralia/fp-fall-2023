import qualified Data.List as L (foldl')

data IsPresent = Yes | No
  deriving (Show)

instance Eq IsPresent where
    (==) :: IsPresent -> IsPresent -> Bool
    Yes == Yes = True
    No  == No  = True
    _   == _   = False

data Eq' a = MkEq {  eq :: a -> a -> Bool
                  , neq :: a -> a -> Bool
                  }

dEqBool :: Eq' IsPresent
dEqBool = MkEq eq' neq'
  where
    eq', neq' :: IsPresent -> IsPresent -> Bool
    Yes `eq'` Yes = True
    No  `eq'` No  = True
    _   `eq'` _   = False

    neq' x y = not $ eq' x y

-------------------------------------------------------------------------------

data User = User { name      :: String
                 , isPresent :: IsPresent }

isClone :: User -> User -> Bool
isClone x y = name x == name y && isPresent x == isPresent y

isClone' :: Eq' IsPresent -> User -> User -> Bool
isClone' eqBool x y = name x == name y && eq eqBool (isPresent x) (isPresent y)

-------------------------------------------------------------------------------

-- https://pointfree.io/
pointed :: (c -> d -> e) -> (a -> b -> c) -> a -> b -> d -> e
-- pointed f g x y z = f (g x y) z
-- pointed f g x y   = f (g x y)        -- eta-reduction
-- pointed f g x y   = f ((g x) y)      -- partial application of `g`
-- pointed f g x y   = f $ (g x) y      -- remove braces with $
-- pointed f g x y   = f . g x $ y      -- use composition .
-- pointed f g x     = f . g x          -- eta-reduction
-- pointed f g x     = (.) f (g x)      -- . is a usual function
-- pointed f g x     = ((.) f) (g x)    -- ...
-- pointed f g x     = ((.) f) $ g x    -- remove braces with $
-- pointed f g x     = ((.) f) . g $ x  -- use composition .
-- pointed f g       = ((.) f) . g      -- eta-reduction
------------------------------------------------------------
-- YOU ARE HERE
------------------------------------------------------------
-- pointed f g       = (.) ((.) f) g    -- . is a usual function
-- pointed f         = (.) ((.) f)      -- eta-reduction
-- pointed f         = (.) ((.) f)      -- eta-reduction
-- pointed f         = (.) $ (.) f      -- remove braces with $
-- pointed f         = (.) $ (.) f      -- remove braces with $
-- pointed f         = (.) . (.) $ f    -- use composition .
pointed           = (.) . (.)           -- eta-reduction
