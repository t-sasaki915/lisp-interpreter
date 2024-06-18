{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispPredefMathsLogic where

import LispData
import LispError (LispError(..))
import LispPredefUtil (lispSimpleFunction, LispFuncProg)
import ListExtra (isPrimitive, isEverythingSame)

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
    sequence $
        mapM expectNumber args >>=
            (\case
                [] -> Right $ return (st, LispNumber ind 1)
                xs -> Right $ return (st, LispNumber ind (product xs))
            )

lispAddition :: LispFuncProg
lispAddition ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                [] -> Right $ return (st, LispNumber ind 0)
                xs -> Right $ return (st, LispNumber ind (sum xs))
            )

lispSubtraction :: LispFuncProg
lispSubtraction ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                []  -> Left $ TooFewArguments ind 1
                [x] -> Right $ return (st, LispNumber ind (negate x))
                xs  -> Right $ return (st, LispNumber ind (foldl1 (-) xs))
            )

lispDivision :: LispFuncProg
lispDivision ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                []  -> Left $ TooFewArguments ind 1
                [x] -> Right $ return (st, LispNumber ind (1 `div` x))
                xs  -> Right $ return (st, LispNumber ind (foldl1 div xs))
            )

lispNotEqual :: LispFuncProg
lispNotEqual ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                []  -> Left $ TooFewArguments ind 1
                xs  -> Right $ return (st, LispBool ind (isPrimitive xs))
            )

lispOnePlus :: LispFuncProg
lispOnePlus ind _ args | length args > 1 =
    return $ Left (TooManyArguments ind 1)
lispOnePlus ind _ args | null args =
    return $ Left (TooFewArguments ind 1)
lispOnePlus ind st args =
    sequence $
        expectNumber (head args) >>=
            (\x -> Right $ return (st, LispNumber ind (x + 1)))

lispOneMinus :: LispFuncProg
lispOneMinus ind _ args | length args > 1 =
    return $ Left (TooManyArguments ind 1)
lispOneMinus ind _ args | null args =
    return $ Left (TooFewArguments ind 1)
lispOneMinus ind st args =
    sequence $
        expectNumber (head args) >>=
            (\x -> Right $ return (st, LispNumber ind (x - 1)))

lispLessThan :: LispFuncProg
lispLessThan ind _ args | length args > 2 =
    return $ Left (TooManyArguments ind 2)
lispLessThan ind _ args | length args < 2 =
    return $ Left (TooFewArguments ind 2)
lispLessThan ind st args =
    sequence $
        expectNumber (head args) >>=
            (\a ->
                expectNumber (args !! 1) >>=
                    (\b ->
                        Right $ return (st, LispBool ind (a < b))
                    )
            )

lispLessThanOrEq :: LispFuncProg
lispLessThanOrEq ind _ args | length args > 2 =
    return $ Left (TooManyArguments ind 2)
lispLessThanOrEq ind _ args | length args < 2 =
    return $ Left (TooFewArguments ind 2)
lispLessThanOrEq ind st args =
    sequence $
        expectNumber (head args) >>=
            (\a ->
                expectNumber (args !! 1) >>=
                    (\b ->
                        Right $ return (st, LispBool ind (a <= b))
                    )
            )

lispEquals :: LispFuncProg
lispEquals ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                [] -> Left $ TooFewArguments ind 1
                xs -> Right $ return (st, LispBool ind (isEverythingSame xs))
            )

lispGreaterThan :: LispFuncProg
lispGreaterThan ind _ args | length args > 2 =
    return $ Left (TooManyArguments ind 2)
lispGreaterThan ind _ args | length args < 2 =
    return $ Left (TooFewArguments ind 2)
lispGreaterThan ind st args =
    sequence $
        expectNumber (head args) >>=
            (\a ->
                expectNumber (args !! 1) >>=
                    (\b ->
                        Right $ return (st, LispBool ind (a > b))
                    )
            )

lispGreaterThanOrEq :: LispFuncProg
lispGreaterThanOrEq ind _ args | length args > 2 =
    return $ Left (TooManyArguments ind 2)
lispGreaterThanOrEq ind _ args | length args < 2 =
    return $ Left (TooFewArguments ind 2)
lispGreaterThanOrEq ind st args =
    sequence $
        expectNumber (head args) >>=
            (\a ->
                expectNumber (args !! 1) >>=
                    (\b ->
                        Right $ return (st, LispBool ind (a >= b))
                    )
            )

lispAnd :: LispFuncProg
lispAnd ind st args =
    sequence $
        mapM expectBool args >>=
            (\case
                [] -> Right $ return (st, LispBool ind True)
                xs -> Right $ return (st, LispBool ind (and xs))
            )

lispEq :: LispFuncProg
lispEq ind _ args | length args > 2 =
    return $ Left (TooManyArguments ind 2)
lispEq ind _ args | length args < 2 =
    return $ Left (TooFewArguments ind 2)
lispEq ind st args =
    return $ Right (st, LispBool ind (head args == last args))

lispEqual :: LispFuncProg
lispEqual ind _ args | length args > 2 =
    return $ Left (TooManyArguments ind 2)
lispEqual ind _ args | length args < 2 =
    return $ Left (TooFewArguments ind 2)
lispEqual ind st args =
    return $ Right (st, LispBool ind (head args == last args))

lispMax :: LispFuncProg
lispMax ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                [] -> Left $ TooFewArguments ind 1
                xs -> Right $ return (st, LispNumber ind (maximum xs))
            )

lispMin :: LispFuncProg
lispMin ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                [] -> Left $ TooFewArguments ind 1
                xs -> Right $ return (st, LispNumber ind (minimum xs))
            )

lispNot :: LispFuncProg
lispNot ind _ args | length args > 1 =
    return $ Left (TooManyArguments ind 1)
lispNot ind _ args | null args =
    return $ Left (TooFewArguments ind 1)
lispNot ind st args =
    sequence $
        expectBool (head args) >>=
            (\a -> Right $ return (st, LispBool ind (not a)))

lispOr :: LispFuncProg
lispOr ind st args =
    sequence $
        mapM expectBool args >>=
            (\case
                [] -> Right $ return (st, LispBool ind False)
                xs -> Right $ return (st, LispBool ind (or xs))
            )

lispZerop :: LispFuncProg
lispZerop ind _ args | length args > 1 =
    return $ Left (TooManyArguments ind 1)
lispZerop ind _ args | null args =
    return $ Left (TooFewArguments ind 1)
lispZerop ind st args =
    sequence $
        expectNumber (head args) >>=
            (\a -> Right $ return (st, LispBool ind (a == 0)))
