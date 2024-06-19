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
lispFormat ind st args
    | length args < 2 = throwE (TooFewArguments ind 2)
    | otherwise       = do
        mode <- expectBoolT (head args)
        fmt  <- exceptT $ lispFormatString ind (args !! 1) (drop 2 args)
        if mode then
            lift (putStr fmt) >> return (st, LispBool ind False)
        else
            return (st, LispString ind fmt)

lispWriteString :: LispFuncProg
lispWriteString ind st args
    | length args > 1 = throwE (TooManyArguments ind 1)
    | null args       = throwE (TooFewArguments ind 1)
    | otherwise       =
        expectStringT (head args) >>=
            (\a -> lift (putStrLn a) >> return (st, LispString ind a))
