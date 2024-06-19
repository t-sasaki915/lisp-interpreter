{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispPredefMathsLogic where

import LispData
import LispError (LispError(..))
import LispPredefUtil (lispSimpleFunction, LispFuncProg)
import ListExtra (isPrimitive, isEverythingSame)

import Control.Monad.Trans.Except (throwE)

lispPredefFuncsMathsLogic :: [LispData]
lispPredefFuncsMathsLogic =
    [ lispSimpleFunction "*"     lispMultiplication
    , lispSimpleFunction "+"     lispAddition
    , lispSimpleFunction "-"     lispSubtraction
    , lispSimpleFunction "/"     lispDivision
    , lispSimpleFunction "/="    lispNotEqual
    , lispSimpleFunction "1+"    lispOnePlus
    , lispSimpleFunction "1-"    lispOneMinus
    , lispSimpleFunction "<"     lispLessThan
    , lispSimpleFunction "<="    lispLessThanOrEq
    , lispSimpleFunction "="     lispEquals
    , lispSimpleFunction ">"     lispGreaterThan
    , lispSimpleFunction ">="    lispGreaterThanOrEq
    , lispSimpleFunction "and"   lispAnd
    , lispSimpleFunction "eq"    lispEq
    , lispSimpleFunction "equal" lispEqual
    , lispSimpleFunction "max"   lispMax
    , lispSimpleFunction "min"   lispMin
    , lispSimpleFunction "not"   lispNot
    , lispSimpleFunction "or"    lispOr
    , lispSimpleFunction "zerop" lispZerop
    ]

lispMultiplication :: LispFuncProg
lispMultiplication ind st args =
    mapM expectNumberT args >>=
        (\case
            [] -> return (st, LispNumber ind 1)
            xs -> return (st, LispNumber ind (product xs))
        )

lispAddition :: LispFuncProg
lispAddition ind st args =
    mapM expectNumberT args >>=
        (\case
            [] -> return (st, LispNumber ind 0)
            xs -> return (st, LispNumber ind (sum xs))
        )

lispSubtraction :: LispFuncProg
lispSubtraction ind st args =
    mapM expectNumberT args >>=
        (\case
            []  -> throwE (TooFewArguments ind 1)
            [x] -> return (st, LispNumber ind (negate x))
            xs  -> return (st, LispNumber ind (foldl1 (-) xs))
        )

lispDivision :: LispFuncProg
lispDivision ind st args =
    mapM expectNumberT args >>=
        (\case
            []  -> throwE (TooFewArguments ind 1)
            [x] -> return (st, LispNumber ind (1 `div` x))
            xs  -> return (st, LispNumber ind (foldl1 div xs))
        )

lispNotEqual :: LispFuncProg
lispNotEqual ind st args =
    mapM expectNumberT args >>=
        (\case
            [] -> throwE (TooFewArguments ind 1)
            xs -> return (st, LispBool ind (isPrimitive xs))
        )

lispOnePlus :: LispFuncProg
lispOnePlus ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispOnePlus ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispOnePlus ind st args =
    expectNumberT (head args) >>=
        (\x -> return (st, LispNumber ind (x + 1)))

lispOneMinus :: LispFuncProg
lispOneMinus ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispOneMinus ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispOneMinus ind st args =
    expectNumberT (head args) >>=
        (\x -> return (st, LispNumber ind (x - 1)))

lispLessThan :: LispFuncProg
lispLessThan ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispLessThan ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispLessThan ind st args = do
    a <- expectNumberT (head args)
    b <- expectNumberT (args !! 1)
    return (st, LispBool ind (a < b))

lispLessThanOrEq :: LispFuncProg
lispLessThanOrEq ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispLessThanOrEq ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispLessThanOrEq ind st args = do
    a <- expectNumberT (head args)
    b <- expectNumberT (args !! 1)
    return (st, LispBool ind (a <= b))

lispEquals :: LispFuncProg
lispEquals ind st args =
    mapM expectNumberT args >>=
        (\case
            [] -> throwE (TooFewArguments ind 1)
            xs -> return (st, LispBool ind (isEverythingSame xs))
        )

lispGreaterThan :: LispFuncProg
lispGreaterThan ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispGreaterThan ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispGreaterThan ind st args = do
    a <- expectNumberT (head args)
    b <- expectNumberT (args !! 1)
    return (st, LispBool ind (a > b))

lispGreaterThanOrEq :: LispFuncProg
lispGreaterThanOrEq ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispGreaterThanOrEq ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispGreaterThanOrEq ind st args = do
    a <- expectNumberT (head args)
    b <- expectNumberT (args !! 1)
    return (st, LispBool ind (a >= b))

lispAnd :: LispFuncProg
lispAnd ind st args =
    mapM expectBoolT args >>=
        (\case
            [] -> return (st, LispBool ind True)
            xs -> return (st, LispBool ind (and xs))
        )

lispEq :: LispFuncProg
lispEq ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispEq ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispEq ind st args =
    return (st, LispBool ind (head args == last args))

lispEqual :: LispFuncProg
lispEqual ind _ args | length args > 2 =
    throwE (TooManyArguments ind 2)
lispEqual ind _ args | length args < 2 =
    throwE (TooFewArguments ind 2)
lispEqual ind st args =
    return (st, LispBool ind (head args == last args))

lispMax :: LispFuncProg
lispMax ind st args =
    mapM expectNumberT args >>=
        (\case
            [] -> throwE (TooFewArguments ind 1)
            xs -> return (st, LispNumber ind (maximum xs))
        )

lispMin :: LispFuncProg
lispMin ind st args =
    mapM expectNumberT args >>=
        (\case
            [] -> throwE (TooFewArguments ind 1)
            xs -> return (st, LispNumber ind (minimum xs))
        )

lispNot :: LispFuncProg
lispNot ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispNot ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispNot ind st args =
    expectBoolT (head args) >>=
        (\a -> return (st, LispBool ind (not a)))

lispOr :: LispFuncProg
lispOr ind st args =
    mapM expectBoolT args >>=
        (\case
            [] -> return (st, LispBool ind False)
            xs -> return (st, LispBool ind (or xs))
        )

lispZerop :: LispFuncProg
lispZerop ind _ args | length args > 1 =
    throwE (TooManyArguments ind 1)
lispZerop ind _ args | null args =
    throwE (TooFewArguments ind 1)
lispZerop ind st args =
    expectNumberT (head args) >>=
        (\a -> return (st, LispBool ind (a == 0)))
