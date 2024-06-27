module InteractiveInterpreter (interactiveInterpreter) where

import LispInterpreter (initEnv, interpretLisp)
import LispSystem (LispEnv)
import Parser (parse)

import Control.Monad.Trans.Except (runExcept, runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.List (elemIndices)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

readProgram :: IO String
readProgram = readProgram' ""
    where
        readProgram' buffer = do
            putStr "Lisp> "
            input <- getLine
            let buffer' = if null buffer then input else buffer ++ ('\n' : input)
                numberOfOpen  = length $ elemIndices '(' buffer'
                numberOfClose = length $ elemIndices ')' buffer'
            if numberOfClose >= numberOfOpen
                then return buffer'
                else readProgram' buffer'

interpreter :: LispEnv -> IO ()
interpreter env = do
    src <- readProgram
    case runExcept (parse src) of
        Right parsed -> do
            (result, env') <- runStateT (runExceptT $ interpretLisp parsed) env
            case result of
                Right lastData ->
                    print lastData >>
                        interpreter env'
                
                Left err ->
                    print err >>
                        interpreter env'
        
        Left err ->
            print err >>
                interpreter env

interactiveInterpreter :: IO ()
interactiveInterpreter = do
    hSetBuffering stdout NoBuffering
    putStrLn "Lisp Interpreter is listening to what you type."
    putStrLn "Typing (exit) is the way to exit gracefully."
    putStrLn ""
    interpreter initEnv
