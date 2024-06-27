module FileInterpreter (fileInterpreter) where

import LispInterpreter (interpretLisp, initEnv)
import Parser (parse)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, withExceptT, except, runExcept)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Functor ((<&>))


interpreter :: String -> IO (Either String ())
interpreter path = (<&> fst) $ runStateT' initEnv $ runExceptT $ do
    src    <- lift $ lift $ readFile path

    parsed <- withExceptT show (except $ runExcept $ parse src)
    _      <- withExceptT show (interpretLisp parsed)

    return ()
    where runStateT' s f = runStateT f s

fileInterpreter :: String -> IO ()
fileInterpreter path = interpreter path >>= either putStrLn return
