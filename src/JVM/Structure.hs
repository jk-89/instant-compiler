module JVM.Structure where

import Prelude
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Data.Text.Lazy.Builder


data Exception
    = UndeclaredVariable String

instance Show Exception where
    show (UndeclaredVariable ident) =
        "Undeclared variable: " ++ ident ++ "."


type Env = Map.Map String Int
data CompilerState = CompilerState {
    env :: Env
}

type CompilerMonadGeneral a = StateT CompilerState (ExceptT Exception IO) a
-- Final code is created by Builder data type.
-- Thanks to that string concatenation operation is lazy and will not be O(N^2).
-- Int represents the maximum depth of a stack in the computed expression.
type CompilerMonadExpr = CompilerMonadGeneral (Builder, Int)
type CompilerMonadProg = CompilerMonadGeneral Builder


startingState :: CompilerState
startingState = CompilerState Map.empty


getEnvSize :: CompilerState -> Int
getEnvSize cState = Map.size $ env cState

getIdent :: String -> CompilerState -> Maybe Int
getIdent ident cState = Map.lookup ident $ env cState

insertIdent :: String -> Int -> CompilerState -> CompilerState
insertIdent ident value cState =
    cState {env = Map.insert ident value $ env cState}

newIdent :: String -> Int -> CompilerMonadGeneral ()
newIdent ident value = do
    modify $ insertIdent ident value
