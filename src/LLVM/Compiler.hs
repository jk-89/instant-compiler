module LLVM.Compiler (evalWhole) where

import Prelude
import Control.Monad.State
import Control.Monad.Except
import Data.Text.Lazy.Builder
import LLVM.Structure
import Grammar.AbsInstant


evalWhole :: Program -> IO (Either Exception Builder)
evalWhole program = runExceptT $ evalStateT (evalS program) startingState


class EvalExpr a where
    evalE :: a -> CompilerMonadExpr

class EvalStmt a where
    evalS :: a -> CompilerMonadStmt


-- `lineL` - list, which after concatenation forms a single new line
--           in the final program.
appendLine :: Builder -> [String] -> CompilerMonadStmt
appendLine code lineL = do
    let line = mconcat $ map fromString lineL
    return $ code <> (fromString "    ") <> line <> (fromString "\n")


evalTwoArgExpr :: Exp -> Exp -> String -> CompilerMonadExpr
evalTwoArgExpr expr1 expr2 op = do
    (code1, value1) <- evalE expr1
    (code2, value2) <- evalE expr2
    register <- createRegister
    new_code <- appendLine (code1 <> code2)
        [register, " = ", op, " i32 ", value1, ", ", value2]
    return (new_code, register)


instance EvalExpr Exp where
    evalE (ExpLit n) = return (fromString "", show n)

    evalE (ExpVar (Ident ident)) = do
        cState <- get
        let contains = containsIdent ident cState
        if contains then do
            register <- createRegister
            new_code <- appendLine (fromString "")
                [register, " = load i32, i32* %", ident]
            return (new_code, register)
        else do
            throwError $ UndeclaredVariable ident

    evalE (ExpAdd expr1 expr2) = evalTwoArgExpr expr1 expr2 "add"

    evalE (ExpSub expr1 expr2) = evalTwoArgExpr expr1 expr2 "sub"

    evalE (ExpMul expr1 expr2) = evalTwoArgExpr expr1 expr2 "mul"

    evalE (ExpDiv expr1 expr2) = evalTwoArgExpr expr1 expr2 "sdiv"


instance EvalStmt Stmt where
    evalS (SAss (Ident ident) expr) = do
        (code, value) <- evalE expr
        cState <- get
        let contains = containsIdent ident cState
        if contains then do
            appendLine code ["store i32 ", value, ", i32* %", ident]
        else do
            newIdent $ ident
            new_code <- appendLine code ["%", ident, " = alloca i32"]
            appendLine new_code ["store i32 ", value, ", i32* %", ident]

    evalS (SExp expr) = do
        (code, value) <- evalE expr
        appendLine code ["call void @printInt(i32 ", value, ")"]


-- Printing functions copied from lab2: LLVM.
instance EvalStmt Program where
    evalS (Prog stmts) = do
        let intro = fromString "\
            \@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\n\
            \declare i32 @printf(i8*, ...)\n\n\
            \define void @printInt(i32 %x) {\n\
            \    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n\
            \    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n\
            \    ret void\n\
            \}\n\n\n\
            \define i32 @main() {\n"
        let outro = fromString "\
            \    ret i32 0\n\
            \}\n"
        code <- mapM evalS stmts
        return $ intro <> (mconcat code) <> outro
