module Main where

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Process
import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO as LazyIO (writeFile)
import LLVM.Compiler
import Grammar.ParInstant


printErrorAndExit :: String -> IO ()
printErrorAndExit err = do
    hPutStrLn stderr err
    exitFailure


createFinalFile :: FilePath -> Builder -> IO ()
createFinalFile f llvmProgram = do
    let finalLL = replaceExtension f "ll"
    let finalBC = replaceExtension f "bc"
    LazyIO.writeFile finalLL $ toLazyText llvmProgram
    readProcess "llvm-as" ["-o", finalBC, finalLL] ""
    exitSuccess


compileFile :: FilePath -> IO ()
compileFile f = do
    fileContent <- readFile f
    case pProgram (myLexer fileContent) of
        Left err -> printErrorAndExit $ show err
        Right program -> do
            result <- evalWhole program
            case result of
                Left err ->
                    printErrorAndExit $ show err
                Right llvmProgram ->
                    createFinalFile f llvmProgram


main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> compileFile f
        _ -> printErrorAndExit "Incorrect parameters."
