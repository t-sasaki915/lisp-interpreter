module Main (main) where

import Control.Exception (try)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    sourceOrErr <- try $ readFile (head args) :: IO (Either IOError String)
    case sourceOrErr of
        Right source ->
            putStrLn source

        Left err ->
            print err
