module Main (main) where

import ErrorTrace (traceError)
import ExceptExtra (exceptT)
import LispRunner (runLisp)
import SyntaxAnalyser (syntaxAnalyse)
import Tokeniser (tokenise)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (withExceptT, runExceptT)
import System.Environment (getArgs)

program :: IO (Either String ())
program = runExceptT $ do
    args   <- lift getArgs
    source <- lift $ readFile (head args)

    tokens    <- withExceptT (traceError source) (exceptT $ tokenise source)
    structure <- withExceptT (traceError source) (exceptT $ syntaxAnalyse tokens)
    _         <- withExceptT (traceError source) (runLisp structure)

    return ()

main :: IO ()
main = program >>= either putStrLn return
