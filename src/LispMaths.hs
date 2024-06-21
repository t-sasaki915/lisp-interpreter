{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispMaths where

import LispData (LispData(..))
import LispDataExtra
import LispEnv
import LispError (RuntimeError(..))
import Util (getM)

import Control.Monad.Trans.Except (throwE)
import Data.Ratio (numerator, denominator)

lispPredefMathsFunctions :: [(String, LispEnvData)]
lispPredefMathsFunctions =
    [ ("*", LispProcedure lispMultiple)
    ]

finaliseRatCalc :: Int -> Rational -> LispData
finaliseRatCalc ind r = case denominator r of
    1 -> LispInteger ind (fromIntegral (numerator r))
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
