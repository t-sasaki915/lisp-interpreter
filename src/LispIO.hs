{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispIO where

import LispError (RuntimeError(..))
import LispSystem

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

lispPredefIOFunctions :: [(String, LispData)]
lispPredefIOFunctions =
    [ ("DISPLAY", LispFunction lispDISPLAY)
    ]

lispDISPLAY :: Procedure
lispDISPLAY ind args
    | length args > 1 = throwE (TooManyArguments ind "DISPLAY" 1)
    | null args       = throwE (TooFewArguments ind "DISPLAY" 1)
    | otherwise       = do
        _ <- lift $ lift $ print (head args)
        return (LispBool ind False)
