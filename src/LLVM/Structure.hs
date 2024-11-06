module LLVM.Structure where

import Prelude
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Data.Text.Lazy.Builder


data Exception
    = UndeclaredVariable String

instance Show Exception where
    show (UndeclaredVariable ident) =
        "Undeclared variable: " ++ ident ++ "."


type Env = Set.Set String
data CompilerState = CompilerState {
    env :: Env,
    usedRegisters :: Int
}

type CompilerMonadGeneral a = StateT CompilerState (ExceptT Exception IO) a
-- Final code is created by Builder data type.
-- Thanks to that string concatenation operation is lazy and will not be O(N^2).
type CompilerMonadExpr = CompilerMonadGeneral (Builder, String)
type CompilerMonadStmt = CompilerMonadGeneral Builder


startingState :: CompilerState
startingState = CompilerState Set.empty 0


insertValue :: String -> CompilerState -> CompilerState
insertValue ident cState =
    cState {env = Set.insert ident $ env cState}

newIdent :: String -> CompilerMonadGeneral ()
newIdent ident = do
    modify $ insertValue ident

containsIdent :: String -> CompilerState -> Bool
containsIdent ident cState = Set.member ident $ env cState


increaseRegisterCount :: Int -> CompilerState -> CompilerState
increaseRegisterCount registers cState =
    cState {usedRegisters = registers + 1}

createRegister :: CompilerMonadGeneral String
createRegister = do
    registers <- gets usedRegisters
    modify $ increaseRegisterCount registers
    return $ "%tmp_" ++ (show registers)
