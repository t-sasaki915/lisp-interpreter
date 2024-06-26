module Main (main) where

import LispInterpreter (initEnv, interpretLisp)
import LispSystem (LispEnv)
import Parser (parse)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (withExceptT, runExceptT, except, runExcept)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import System.Environment (getArgs)

program :: StateT LispEnv IO (Either String ())
program = runExceptT $ do
    args   <- lift2 getArgs
    src    <- lift2 $ readFile (head args)

    parsed <- withExceptT show (exceptT $ parse src)
    result <- withExceptT show (interpretLisp parsed)

    _      <- lift2 $ print result

    return ()
    where
        lift2 = lift . lift
        exceptT = except . runExcept

main :: IO ()
main =
    runStateT program initEnv >>=
        (either putStrLn return . fst)
