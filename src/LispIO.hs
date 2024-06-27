{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispIO where

import LispError (RuntimeError(..))
import LispOperation
import LispSystem
import Util ((~>), getOrElseM)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import System.Exit (ExitCode (..), exitWith)

lispPredefIOFunctions :: [(String, LispEnvData)]
lispPredefIOFunctions =
    [ "DISPLAY" ~> LispFunction lispDISPLAY
    , "EXIT"    ~> LispFunction lispEXIT
    ]

lispDISPLAY :: Procedure
lispDISPLAY args
    | length args > 1 = throwE (TooManyArguments "DISPLAY" 1)
    | null args       = throwE (TooFewArguments "DISPLAY" 1)
    | otherwise       = do
        _ <- lift $ lift $ print (head args)
        return (LispBool False)

lispEXIT :: Procedure
lispEXIT args
    | length args > 1 = throwE (TooManyArguments "EXIT" 1)
    | otherwise       = do
        exitCode <- getOrElseM args 0 (LispInteger 0) >>= treatAsLispInteger
        let haskellExitCode = case exitCode of
                0 -> ExitSuccess
                n -> ExitFailure (fromIntegral n)
        _ <- lift $ lift $ exitWith haskellExitCode

        return (LispBool False)
