module Main (main) where

import FileInterpreter (fileInterpreter)
import InteractiveInterpreter (interactiveInterpreter)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> interactiveInterpreter
        xs -> fileInterpreter (head xs)
