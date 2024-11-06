module JVM.Compiler (evalWhole) where

import Prelude
import Control.Monad.State
import Control.Monad.Except
import Data.Text.Lazy.Builder
import JVM.Structure
import Grammar.AbsInstant


evalWhole :: Program -> IO (Either Exception Builder)
evalWhole program = runExceptT $ evalStateT (evalProgram program) startingState


class Eval a where
    eval :: a -> CompilerMonadExpr

class EvalProgram a where
    evalProgram :: a -> CompilerMonadProg


wrapLine :: String -> Builder
wrapLine line = mconcat $ map fromString ["    ", line, "\n"]


evalTwoArgExpr :: Exp -> Exp -> String -> Bool -> CompilerMonadExpr
evalTwoArgExpr expr1 expr2 op shouldSwap = do
    (code1, depth1) <- eval expr1
    (code2, depth2) <- eval expr2
    if depth1 < depth2 then do
        let swap = case shouldSwap of
                True -> wrapLine "swap"
                False -> fromString ""
        return (code2 <> code1 <> swap <> (wrapLine op), max (depth1 + 1) depth2)
    else
        return (code1 <> code2 <> (wrapLine op), max depth1 (depth2 + 1))


getLitInstruction :: Integer -> String
getLitInstruction n
    | n == -1                  = "iconst_m1"
    | 0 <= n && n <= 5         = "iconst_" ++ show n
    | -128 <= n && n < 128     = "bipush " ++ show n
    | -32768 <= n && n < 32768 = "sipush " ++ show n
    | otherwise                = "ldc " ++ show n

getVarInstruction :: Int -> String
getVarInstruction n
    | 0 <= n && n <= 3 = "iload_" ++ show n
    | otherwise        = "iload " ++ show n


instance Eval Exp where
    eval (ExpLit n) =
        return (wrapLine $ getLitInstruction n, 1)

    eval (ExpVar (Ident ident)) = do
        cState <- get
        let res = getIdent ident cState
        case res of
            Just value ->
                return (wrapLine $ getVarInstruction value, 1)
            Nothing ->
                throwError $ UndeclaredVariable ident

    eval (ExpAdd expr1 expr2) = evalTwoArgExpr expr1 expr2 "iadd" False

    eval (ExpSub expr1 expr2) = evalTwoArgExpr expr1 expr2 "isub" True

    eval (ExpMul expr1 expr2) = evalTwoArgExpr expr1 expr2 "imul" False

    eval (ExpDiv expr1 expr2) = evalTwoArgExpr expr1 expr2 "idiv" True


getAssInstruction :: Int -> String
getAssInstruction n
    | 0 <= n && n <= 3 = "istore_" ++ show n
    | otherwise        = "istore " ++ show n


instance Eval Stmt where
    eval (SAss (Ident ident) expr) = do
        (code, depth) <- eval expr
        cState <- get
        let contains = getIdent ident cState
        let getValue = \(contain) -> (
                case contain of
                    Nothing -> do
                        let size = getEnvSize cState
                        newIdent ident size
                        return size
                    Just value ->
                        return value
                )
        value <- getValue contains
        return (code <> (wrapLine $ getAssInstruction value), depth)

    eval (SExp expr) = do
        (code, depth) <- eval expr
        return (
            (wrapLine "getstatic java/lang/System/out Ljava/io/PrintStream;") <>
            code <>
            (wrapLine "invokevirtual java/io/PrintStream/println(I)V"),
            depth + 1)


instance EvalProgram Program where
    evalProgram (Prog stmts) = do
        results <- mapM eval stmts
        cState <- get
        let size = getEnvSize cState
        let (codes, depths) = unzip results
        let (code, depth) = (mconcat codes, maximum $ 0 : depths)
        -- We set "limit locals" to "size + 1" because main() has one argument.
        let intro = mconcat $ map fromString ["\
                \.super java/lang/Object\n\n\
                \.method public <init>()V\n\
                \    aload_0\n\
                \    invokespecial java/lang/Object/<init>()V\n\
                \    return\n\
                \.end method\n\n\
                \.method public static main([Ljava/lang/String;)V\n\
                \.limit locals ",
                show (size + 1),
                "\n.limit stack ",
                show depth,
                "\n"]
        let outro = fromString "\
            \    return\n\
            \.end method\n"
        return $ intro <> code <> outro
