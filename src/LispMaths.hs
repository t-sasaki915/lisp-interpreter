{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispMaths where

import LispError (RuntimeError(..))
import LispOperation
import LispSystem
import Util ((~>))

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))
import Data.Ratio (numerator, denominator)
import GHC.Float (powerFloat)

lispPredefMathsFunctions :: [(String, LispEnvData)]
lispPredefMathsFunctions =
    [ "*"       ~> LispFunction lispMultiple
    , "+"       ~> LispFunction lispAddition
    , "-"       ~> LispFunction lispSubtract
    , "/"       ~> LispFunction lispDivision
    , "<"       ~> LispFunction lispLessThan
    , "<="      ~> LispFunction lispLessThanOrEq
    , "="       ~> LispFunction lispNumberEq
    , ">"       ~> LispFunction lispGreaterThan
    , ">="      ~> LispFunction lispGreaterThanOrEq
    , "ABS"     ~> LispFunction lispABS
    , "COS"     ~> LispFunction lispCOS
    , "EXPT"    ~> LispFunction lispEXPT
    , "MAX"     ~> LispFunction lispMAX
    , "MIN"     ~> LispFunction lispMIN
    , "NOT"     ~> LispFunction lispNOT
    , "SIN"     ~> LispFunction lispSIN
    , "SQRT"    ~> LispFunction lispSQRT
    , "NUMBERP" ~> LispFunction lispNUMBERP
    ]

guaranteeNotZero :: LispNumber -> Execution ()
guaranteeNotZero (LispInteger' n)  | n /= 0 = return ()
guaranteeNotZero (LispRational' r) | r /= 0 = return ()
guaranteeNotZero (LispReal' r)     | r /= 0 = return ()
guaranteeNotZero _ = throwE ZeroDivideCalculation

power :: LispNumber -> LispNumber -> LispNumber
power (LispReal' base) (LispReal' pow) =
    LispReal' (powerFloat base pow)
power (LispReal' base) (LispRational' pow) =
    power (LispReal' base) (LispReal' (fromRational pow))
power (LispReal' base) (LispInteger' pow) =
    power (LispReal' base) (LispReal' (fromIntegral pow))
power (LispRational' base) (LispReal' pow) =
    power (LispReal' (fromRational base)) (LispReal' pow)
power (LispRational' base) (LispRational' pow) =
    case (numerator pow, denominator pow) of
        (a, 1) -> power (LispRational' base) (LispInteger' a)
        _      -> power (LispReal' (fromRational base)) (LispReal' (fromRational pow))
power (LispRational' base) (LispInteger' pow) =
    case (numerator base, denominator base) of
        (a, 1) -> power (LispInteger' a) (LispInteger' pow)
        (a, b) -> power (LispInteger' a) (LispInteger' pow) /
                    power (LispInteger' b) (LispInteger' pow)
power (LispInteger' base) (LispReal' pow) =
    power (LispReal' (fromIntegral base)) (LispReal' pow)
power (LispInteger' base) (LispRational' pow) =
    case (numerator pow, denominator pow) of
        (a, 1) -> power (LispInteger' base) (LispInteger' a)
        _      -> power (LispReal' (fromIntegral base)) (LispReal' (fromRational pow))
power (LispInteger' base) (LispInteger' pow)
    | pow < 0   = LispInteger' 1 / power (LispInteger' base) (LispInteger' (abs pow))
    | otherwise = LispInteger' (base ^ pow)

lispMultiple :: Procedure
lispMultiple args =
    mapM treatAsLispNumber args >>=
        (fromLispNumber . product)

lispAddition :: Procedure
lispAddition args =
    mapM treatAsLispNumber args >>=
        (fromLispNumber . sum)

lispSubtract :: Procedure
lispSubtract args
    | null args        = throwE (TooFewArguments "-" 1)
    | length args == 1 =
        treatAsLispNumber (head args) >>=
            (fromLispNumber . negate)
    | otherwise        =
        mapM treatAsLispNumber args >>=
            (fromLispNumber . foldl1 (-))

lispDivision :: Procedure
lispDivision args
    | null args        = throwE (TooFewArguments "/" 1)
    | length args == 1 =
        treatAsLispNumber (head args) >>= (\a ->
            guaranteeNotZero a >>=
                const (fromLispNumber (LispInteger' 1 / a)))
    | otherwise        =
        mapM treatAsLispNumber args >>= (\nums ->
            mapM guaranteeNotZero (tail nums) >>=
                const (fromLispNumber (foldl1 (/) nums)))

lispLessThan :: Procedure
lispLessThan args
    | null args        = throwE (TooFewArguments "<" 1)
    | length args == 1 = return (LispBool True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) < var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool res)

lispLessThanOrEq :: Procedure
lispLessThanOrEq args
    | null args        = throwE (TooFewArguments "<=" 1)
    | length args == 1 = return (LispBool True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) <= var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool res)

lispNumberEq :: Procedure
lispNumberEq args
    | null args        = throwE (TooFewArguments "=" 1)
    | length args == 1 = return (LispBool True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) == var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool res)

lispGreaterThan :: Procedure
lispGreaterThan args
    | null args        = throwE (TooFewArguments ">" 1)
    | length args == 1 = return (LispBool True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) > var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool res)

lispGreaterThanOrEq :: Procedure
lispGreaterThanOrEq args
    | null args        = throwE (TooFewArguments ">=" 1)
    | length args == 1 = return (LispBool True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) >= var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool res)

lispABS :: Procedure
lispABS args
    | length args > 1 = throwE (TooManyArguments "ABS" 1)
    | null args       = throwE (TooFewArguments "ABS" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber . abs)

lispCOS :: Procedure
lispCOS args
    | length args > 1 = throwE (TooManyArguments "COS" 1)
    | null args       = throwE (TooFewArguments "COS" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber . LispReal' . cos . toReal)

lispEXPT :: Procedure
lispEXPT args
    | length args > 2 = throwE (TooManyArguments "EXPT" 2)
    | length args < 2 = throwE (TooFewArguments "EXPT" 2)
    | otherwise       = do
        base <- treatAsLispNumber (head args)
        pow  <- treatAsLispNumber (args !! 1)
        fromLispNumber (power base pow)

lispMAX :: Procedure
lispMAX args
    | null args = throwE (TooFewArguments "MAX" 1)
    | otherwise =
        mapM treatAsLispNumber args >>=
            (fromLispNumber . maximum)

lispMIN :: Procedure
lispMIN args
    | null args = throwE (TooFewArguments "MIN" 1)
    | otherwise =
        mapM treatAsLispNumber args >>=
            (fromLispNumber . minimum)

lispNOT :: Procedure
lispNOT args
    | length args > 1 = throwE (TooManyArguments "NOT" 1)
    | null args       = throwE (TooFewArguments "NOT" 1)
    | otherwise       =
        treatAsLispBool (head args) <&> (LispBool . not)

lispSIN :: Procedure
lispSIN args
    | length args > 1 = throwE (TooManyArguments "SIN" 1)
    | null args       = throwE (TooFewArguments "SIN" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber . LispReal' . sin . toReal)

lispSQRT :: Procedure
lispSQRT args
    | length args > 1 = throwE (TooManyArguments "SQRT" 1)
    | null args       = throwE (TooFewArguments "SQRT" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber . LispReal' . sqrt . toReal)

lispNUMBERP :: Procedure
lispNUMBERP args
    | length args > 1 = throwE (TooManyArguments "NUMBERP" 1)
    | null args       = throwE (TooFewArguments "NUMBERP" 1)
    | otherwise       =
        case head args of
            (LispReal _)      -> return (LispBool True)
            (LispRational _ ) -> return (LispBool True)
            (LispInteger _)   -> return (LispBool True)
            _                 -> return (LispBool False)
