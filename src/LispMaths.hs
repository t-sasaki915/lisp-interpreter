{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispMaths where

import LispData (LispData(..), indAndType)
import LispDataExtra
import LispEnv
import LispError (RuntimeError(..))
import Util (getM)

import Control.Monad.Trans.Except (throwE)
import Data.Ratio ((%), numerator, denominator)

lispPredefMathsFunctions :: [(String, LispEnvData)]
lispPredefMathsFunctions =
    [ ("*", LispProcedure lispMultiple)
    , ("+", LispProcedure lispAddition)
    , ("-", LispProcedure lispSubtract)
    , ("/", LispProcedure lispDivision)
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