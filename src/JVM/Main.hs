module Main where

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import System.Process
import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO as LazyIO (writeFile)
import JVM.Compiler
import Grammar.ParInstant


printErrorAndExit :: String -> IO ()
printErrorAndExit err = do
    hPutStrLn stderr err
    exitFailure


appendClassID :: Builder -> String -> Builder
appendClassID code classID =
    (fromString $ ".class public " ++ classID) <> (fromString "\n") <> code


createFinalFile :: FilePath -> Builder -> IO ()
createFinalFile f jvmProgram = do
    let finalJ = replaceExtension f "j"
    let finalDir = takeDirectory f
    LazyIO.writeFile finalJ $ toLazyText jvmProgram
    readProcess "java" ["-jar", "./lib/jasmin.jar", "-d", finalDir, finalJ] ""
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
                Right jvmProgram ->
                    createFinalFile f $ appendClassID jvmProgram $ takeBaseName f


main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> compileFile f
        _ -> printErrorAndExit "Incorrect parameters."
