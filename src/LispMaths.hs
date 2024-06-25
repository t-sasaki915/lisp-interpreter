{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispMaths where

import LispError (RuntimeError(..))
import LispOperation
import LispSystem

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))
import Data.Ratio (numerator, denominator)
import GHC.Float (powerFloat)

lispPredefMathsFunctions :: [(String, LispData)]
lispPredefMathsFunctions =
    [ ("*",       LispFunction lispMultiple)
    , ("+",       LispFunction lispAddition)
    , ("-",       LispFunction lispSubtract)
    , ("/",       LispFunction lispDivision)
    , ("<",       LispFunction lispLessThan)
    , ("<=",      LispFunction lispLessThanOrEq)
    , ("=",       LispFunction lispNumberEq)
    , (">",       LispFunction lispGreaterThan)
    , (">=",      LispFunction lispGreaterThanOrEq)
    , ("ABS",     LispFunction lispABS)
    , ("COS",     LispFunction lispCOS)
    , ("EXPT",    LispFunction lispEXPT)
    , ("MAX",     LispFunction lispMAX)
    , ("MIN",     LispFunction lispMIN)
    , ("NOT",     LispFunction lispNOT)
    , ("SIN",     LispFunction lispSIN)
    , ("SQRT",    LispFunction lispSQRT)
    , ("NUMBER?", LispFunction lispNUMBERP)
    ]

guaranteeNotZero :: Int -> LispNumber -> Execution ()
guaranteeNotZero _ (LispInteger' n)  | n /= 0 = return ()
guaranteeNotZero _ (LispRational' r) | r /= 0 = return ()
guaranteeNotZero _ (LispReal' r)     | r /= 0 = return ()
guaranteeNotZero n _ = throwE (ZeroDivideCalculation n)

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
lispMultiple ind args =
    mapM treatAsLispNumber args >>=
        (fromLispNumber ind . product)

lispAddition :: Procedure
lispAddition ind args =
    mapM treatAsLispNumber args >>=
        (fromLispNumber ind . sum)

lispSubtract :: Procedure
lispSubtract ind args
    | null args        = throwE (TooFewArguments ind "-" 1)
    | length args == 1 =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . negate)
    | otherwise        =
        mapM treatAsLispNumber args >>=
            (fromLispNumber ind . foldl1 (-))

lispDivision :: Procedure
lispDivision ind args
    | null args        = throwE (TooFewArguments ind "/" 1)
    | length args == 1 =
        treatAsLispNumber (head args) >>= (\a ->
            guaranteeNotZero ind a >>=
                const (fromLispNumber ind (LispInteger' 1 / a)))
    | otherwise        =
        mapM treatAsLispNumber args >>= (\nums ->
            mapM (guaranteeNotZero ind) (tail nums) >>=
                const (fromLispNumber ind (foldl1 (/) nums)))

lispLessThan :: Procedure
lispLessThan ind args
    | null args        = throwE (TooFewArguments ind "<" 1)
    | length args == 1 = return (LispBool ind True)
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
        return (LispBool ind res)

lispLessThanOrEq :: Procedure
lispLessThanOrEq ind args
    | null args        = throwE (TooFewArguments ind "<=" 1)
    | length args == 1 = return (LispBool ind True)
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
        return (LispBool ind res)

lispNumberEq :: Procedure
lispNumberEq ind args
    | null args        = throwE (TooFewArguments ind "=" 1)
    | length args == 1 = return (LispBool ind True)
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
        return (LispBool ind res)

lispGreaterThan :: Procedure
lispGreaterThan ind args
    | null args        = throwE (TooFewArguments ind ">" 1)
    | length args == 1 = return (LispBool ind True)
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
        return (LispBool ind res)

lispGreaterThanOrEq :: Procedure
lispGreaterThanOrEq ind args
    | null args        = throwE (TooFewArguments ind ">=" 1)
    | length args == 1 = return (LispBool ind True)
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
        return (LispBool ind res)

lispABS :: Procedure
lispABS ind args
    | length args > 1 = throwE (TooManyArguments ind "ABS" 1)
    | null args       = throwE (TooFewArguments ind "ABS" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . abs)

lispCOS :: Procedure
lispCOS ind args
    | length args > 1 = throwE (TooManyArguments ind "COS" 1)
    | null args       = throwE (TooFewArguments ind "COS" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . LispReal' . cos . toReal)

lispEXPT :: Procedure
lispEXPT ind args
    | length args > 2 = throwE (TooManyArguments ind "EXPT" 2)
    | length args < 2 = throwE (TooFewArguments ind "EXPT" 2)
    | otherwise       = do
        base <- treatAsLispNumber (head args)
        pow  <- treatAsLispNumber (args !! 1)
        fromLispNumber ind (power base pow)

lispMAX :: Procedure
lispMAX ind args
    | null args = throwE (TooFewArguments ind "MAX" 1)
    | otherwise =
        mapM treatAsLispNumber args >>=
            (fromLispNumber ind . maximum)

lispMIN :: Procedure
lispMIN ind args
    | null args = throwE (TooFewArguments ind "MIN" 1)
    | otherwise =
        mapM treatAsLispNumber args >>=
            (fromLispNumber ind . minimum)

lispNOT :: Procedure
lispNOT ind args
    | length args > 1 = throwE (TooManyArguments ind "NOT" 1)
    | null args       = throwE (TooFewArguments ind "NOT" 1)
    | otherwise       =
        treatAsLispBool (head args) <&> (LispBool ind . not)

lispSIN :: Procedure
lispSIN ind args
    | length args > 1 = throwE (TooManyArguments ind "SIN" 1)
    | null args       = throwE (TooFewArguments ind "SIN" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . LispReal' . sin . toReal)

lispSQRT :: Procedure
lispSQRT ind args
    | length args > 1 = throwE (TooManyArguments ind "SQRT" 1)
    | null args       = throwE (TooFewArguments ind "SQRT" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . LispReal' . sqrt . toReal)

lispNUMBERP :: Procedure
lispNUMBERP ind args
    | length args > 1 = throwE (TooManyArguments ind "NUMBER?" 1)
    | null args       = throwE (TooFewArguments ind "NUMBER?" 1)
    | otherwise       =
        case head args of
            (LispReal _ _)      -> return (LispBool ind True)
            (LispRational _ _ ) -> return (LispBool ind True)
            (LispInteger _ _)   -> return (LispBool ind True)
            _                   -> return (LispBool ind False)
