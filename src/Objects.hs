module Objects where
  
import qualified Data.Map as M


type UserID = Int

data AccessRights = Read | Write | ReadAndWrite | Admin
  deriving (Eq, Show, Ord)

newtype MyState = DB (M.Map UserID [AccessRights])
  deriving (Eq, Show)
