module Main (main) where

import LispEnv (LispEnv)
import LispError (traceError)
import LispInterpreter (initEnv, interpretLisp)
import Parser (parse)
import Util (exceptT, lift2)

import Control.Monad.Trans.Except (withExceptT, runExceptT)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import System.Environment (getArgs)

program :: StateT LispEnv IO (Either String ())
program = runExceptT $ do
    args   <- lift2 getArgs
    src    <- lift2 $ readFile (head args)

    parsed <- withExceptT (traceError src) (exceptT $ parse src)
    result <- withExceptT (traceError src) (interpretLisp parsed)

    _      <- lift2 $ print result

    return ()

main :: IO ()
main =
    runStateT program initEnv >>=
        (either putStrLn return . fst)
