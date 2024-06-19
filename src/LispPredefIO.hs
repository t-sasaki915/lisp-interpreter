{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispPredefIO where

import ExceptExtra (exceptT)
import LispData
import LispError (LispError(..))
import LispFormat (lispFormatString)
import LispPredefUtil (LispFuncProg, lispSimpleFunction)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

lispPredefFuncsIO :: [LispData]
lispPredefFuncsIO =
    [ lispSimpleFunction "format"       lispFormat
    , lispSimpleFunction "write-string" lispWriteString
    ]

lispFormat :: LispFuncProg
lispFormat ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispFormat ind st args = do
    mode      <- expectBoolT (head args)
    formatted <- exceptT $ lispFormatString ind (args !! 1) (drop 2 args)

    if mode then
        lift (putStr formatted) >> return (st, LispBool ind False)
    else
        return (st, LispString ind formatted)

lispWriteString :: LispFuncProg
lispWriteString ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispWriteString ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispWriteString ind st args =
    expectStringT (head args) >>=
        (\a -> lift (putStrLn a) >> return (st, LispString ind a))
