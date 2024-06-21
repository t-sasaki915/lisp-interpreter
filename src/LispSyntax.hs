{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispSyntax where

import Eval (eval)
import LispData (LispData(..))
import LispDataExtra (treatAsLispBool)
import LispEnv
import LispError (RuntimeError(..))
import Util (getM, getOrElseM)

import Control.Monad.Trans.Except (throwE)

lispPredefSyntaxes :: [(String, LispEnvData)]
lispPredefSyntaxes =
    [ ("IF"   , LispSyntax lispIF)
    , ("QUOTE", LispSyntax lispQUOTE)
    ]

lispIF :: Evalable
lispIF ind args
    | length args > 3 = throwE (TooManyArguments ind "IF" 3)
    | length args < 2 = throwE (TooFewArguments ind "IF" 2)
    | otherwise       = do
        cond  <- eval (head args) >>= treatAsLispBool
        body1 <- getM args 1
        body2 <- getOrElseM args 2 (LispBool ind False)

        if cond then eval body1
                else eval body2

lispQUOTE :: Evalable
lispQUOTE ind args
    | length args > 1 = throwE (TooManyArguments ind "QUOTE" 1)
    | null args       = throwE (TooFewArguments ind "QUOTE" 1)
    | otherwise       = getM args 0
