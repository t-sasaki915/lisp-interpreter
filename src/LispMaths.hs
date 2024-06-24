{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispMaths where

import LispData (LispData(..), indAndType)
import LispDataExtra
import LispEnv
import LispError (RuntimeError(..))
import Util (getM)

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))
import Data.Ratio ((%), numerator, denominator)

lispPredefMathsFunctions :: [(String, LispEnvData)]
lispPredefMathsFunctions =
    [ ("*",   LispProcedure lispMultiple)
    , ("+",   LispProcedure lispAddition)
    , ("-",   LispProcedure lispSubtract)
    , ("/",   LispProcedure lispDivision)
    , ("<",   LispProcedure lispLessThan)
    , ("<=",  LispProcedure lispLessThanOrEq)
    , ("=",   LispProcedure lispNumberEq)
    , (">",   LispProcedure lispGreaterThan)
    , (">=",  LispProcedure lispGreaterThanOrEq)
    , ("COS", LispProcedure lispCOS)
    ]

finaliseRatCalc :: Int -> Rational -> LispData
finaliseRatCalc ind r = case denominator r of
    1 -> LispInteger ind (numerator r)
    _ -> LispRational ind r

lispMultiple :: Evalable
lispMultiple ind args
    | null args        = return (LispInteger ind 1)
    | length args == 1 = getM args 0
    | otherwise        = do
        if isThereReal args then do
            vars <- treatAsLispReals args
            return (LispReal ind (product vars))
        else if isThereRat args then do
            vars <- treatAsLispRats args
            return (finaliseRatCalc ind (product vars))
        else do
            vars <- treatAsLispInts args
            return (LispInteger ind (product vars))

lispAddition :: Evalable
lispAddition ind args
    | null args        = return (LispInteger ind 0)
    | length args == 1 = getM args 0
    | otherwise        = do
        if isThereReal args then do
            vars <- treatAsLispReals args
            return (LispReal ind (sum vars))
        else if isThereRat args then do
            vars <- treatAsLispRats args
            return (finaliseRatCalc ind (sum vars))
        else do
            vars <- treatAsLispInts args
            return (LispInteger ind (sum vars))

lispSubtract :: Evalable
lispSubtract ind args
    | null args        = throwE (TooFewArguments ind "-" 1)
    | length args == 1 =
        case head args of
            (LispReal _ r)     -> return (LispReal ind (negate r))
            (LispRational _ r) -> return (LispRational ind (negate r))
            (LispInteger _ n)  -> return (LispInteger ind (negate n))
            d -> throwE (uncurry IncompatibleType (indAndType d) "NUMBER")
    | otherwise        = do
        if isThereReal args then do
            vars <- treatAsLispReals args
            return (LispReal ind (foldl1 (-) vars))
        else if isThereRat args then do
            vars <- treatAsLispRats args
            return (finaliseRatCalc ind (foldl1 (-) vars))
        else do
            vars <- treatAsLispInts args
            return (LispInteger ind (foldl1 (-) vars))

lispDivision :: Evalable
lispDivision ind args
    | null args        = throwE (TooFewArguments ind "/" 1)
    | length args == 1 =
        case head args of
            (LispReal _ r)     -> return (LispReal ind (1 / r))
            (LispRational _ r) -> return (finaliseRatCalc ind (recip r))
            (LispInteger _ n)  -> return (LispRational ind (1 % n))
            d -> throwE (uncurry IncompatibleType (indAndType d) "NUMBER")
    | otherwise        = do
        if isThereReal args then do
            vars <- treatAsLispReals args
            return (LispReal ind (foldl1 (/) vars))
        else if isThereRat args then do
            vars <- treatAsLispRats args
            return (finaliseRatCalc ind (foldl1 (/) vars))
        else do
            vars <- treatAsLispInts args
            let first = head vars % 1
                fracs = map (1 %) (tail vars)
            return (finaliseRatCalc ind (product (first : fracs)))

lispLessThan :: Evalable
lispLessThan ind args
    | null args        = throwE (TooFewArguments ind "<" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- treatAsLispReals args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) < var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispLessThanOrEq :: Evalable
lispLessThanOrEq ind args
    | null args        = throwE (TooFewArguments ind "<=" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- treatAsLispReals args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) <= var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispNumberEq :: Evalable
lispNumberEq ind args
    | null args        = throwE (TooFewArguments ind "=" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- treatAsLispReals args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) == var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispGreaterThan :: Evalable
lispGreaterThan ind args
    | null args        = throwE (TooFewArguments ind ">" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- treatAsLispReals args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) > var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispGreaterThanOrEq :: Evalable
lispGreaterThanOrEq ind args
    | null args        = throwE (TooFewArguments ind ">=" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- treatAsLispReals args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) >= var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispCOS :: Evalable
lispCOS ind args
    | length args > 1 = throwE (TooManyArguments ind "COS" 1)
    | null args       = throwE (TooFewArguments ind "COS" 1)
    | otherwise       =
        treatAsLispReal (head args) <&> (LispReal ind . cos)
