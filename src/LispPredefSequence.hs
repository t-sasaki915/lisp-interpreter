{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispPredefSequence where

import LispData
import LispError (LispError(..))
import LispPredefUtil (lispSimpleFunction, LispFuncProg)
import ListExtra ((!?), dropRight, takeRight)

import Control.Monad.Trans.Except (throwE)
import Data.Maybe (fromMaybe)

lispPredefFuncsSequence :: [LispData]
lispPredefFuncsSequence =
    [ lispSimpleFunction "append"      lispAppend
    , lispSimpleFunction "butlast"     lispButlast
    , lispSimpleFunction "car"         lispCar
    , lispSimpleFunction "cddr"        lispCddr
    , lispSimpleFunction "cdr"         lispCdr
    , lispSimpleFunction "first"       lispCar
    , lispSimpleFunction "getf"        lispGetf
    , lispSimpleFunction "last"        lispLast
    , lispSimpleFunction "list"        lispList
    , lispSimpleFunction "list-length" lispListLength
    ]

lispAppend :: LispFuncProg
lispAppend ind st args =
    mapM expectLazyListT args >>=
        (\case
            [] -> return (st, LispBool ind False)
            xs -> return (st, LispLazyList ind (concat xs))
        )

lispButlast :: LispFuncProg
lispButlast ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispButlast ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispButlast ind st args = do
    lst <- expectLazyListT (head args)
    n   <- expectNumberT (fromMaybe (LispNumber ind 1) (args !? 1))
    if n < 0
        then throwE (IllegalArgument ind "Given number is negative.")
        else return (st, LispLazyList ind (dropRight n lst))

lispCar :: LispFuncProg
lispCar ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispCar ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispCar ind st args =
    expectLazyListT (head args) >>=
        (\case
            [] -> return (st, LispBool ind False)
            xs -> return (st, head xs)
        )

lispCddr :: LispFuncProg
lispCddr ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispCddr ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispCddr ind st args =
    expectLazyListT (head args) >>=
        (\a -> return (st, LispLazyList ind (drop 2 a)))

lispCdr :: LispFuncProg
lispCdr ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispCdr ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispCdr ind st args =
    expectLazyListT (head args) >>=
        (\a -> return (st, LispLazyList ind (drop 1 a)))

lispGetf :: LispFuncProg
lispGetf ind _ args | length args > 3 =
    throwE (TooManyArguments ind 3)
lispGetf ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispGetf ind st args = do
    lst <- expectLazyListT (head args)
    let key = args !! 1
        def = fromMaybe (LispBool ind False) (args !? 2)
    return (st, fromMaybe def (dropWhile (key /=) lst !? 1))

lispLast :: LispFuncProg
lispLast ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispLast ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispLast ind st args = do
    lst <- expectLazyListT (head args)
    n   <- expectNumberT (fromMaybe (LispNumber ind 1) (args !? 1))
    if n < 0
        then throwE (IllegalArgument ind "Given number is negative")
        else return (st, LispLazyList ind (takeRight n lst))

lispList :: LispFuncProg
lispList ind st args =
    return (st, LispLazyList ind args)

lispListLength :: LispFuncProg
lispListLength ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispListLength ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispListLength ind st args =
    expectLazyListT (head args) >>=
        (\a -> return (st, LispNumber ind (length a)))
