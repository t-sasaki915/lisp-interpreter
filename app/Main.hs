module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (withExceptT, runExceptT)
import System.Environment (getArgs)

program :: IO (Either String ())
program = runExceptT $ do
    return ()

main :: IO ()
main = program >>= either putStrLn return
