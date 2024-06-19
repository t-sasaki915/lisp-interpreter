{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispPredefIO where

import LispData
import LispError (LispError(..))
import LispPredefUtil (LispFuncProg, lispSimpleFunction)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

lispPredefFuncsIO :: [LispData]
lispPredefFuncsIO =
    [ lispSimpleFunction "write-string" lispWriteString
    ]

lispWriteString :: LispFuncProg
lispWriteString ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispWriteString ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispWriteString ind st args =
    expectStringT (head args) >>=
        (\a -> lift (putStrLn a) >> return (st, LispString ind a))
