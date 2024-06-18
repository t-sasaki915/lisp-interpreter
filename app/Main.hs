module Main (main) where

import ErrorTrace (traceError)
import LispRunner (runLisp)
import SyntaxAnalyser (syntaxAnalyse)
import Tokeniser (tokenise)

import Control.Exception (try)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    sourceOrErr <- try $ readFile (head args) :: IO (Either IOError String)
    case sourceOrErr of
        Right source ->
            case tokenise source of
                Right tokens ->
                    case syntaxAnalyse tokens of
                        Right program -> do
                            result <- runLisp program
                            case result of
                                Right () ->
                                    return ()
                                    
                                Left err ->
                                    putStrLn $ traceError err source
                        
                        Left err ->
                            putStrLn $ traceError err source
                
                Left err ->
                    putStrLn $ traceError err source
        Left err ->
            print err
