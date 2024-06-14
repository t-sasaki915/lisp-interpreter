module Main (main) where

import ErrorTrace (traceError)
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
                    case syntaxAnalyse source tokens of
                        Right structure ->
                            print structure
                        
                        Left err ->
                            putStrLn $ traceError err
                
                Left err ->
                    putStrLn $ traceError err
        Left err ->
            print err
